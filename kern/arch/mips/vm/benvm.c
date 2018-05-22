/*
 * Copyright (c) 2000, 2001, 2002, 2003, 2004, 2005, 2008, 2009
 *	The President and Fellows of Harvard College.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <types.h>
#include <kern/errno.h>
#include <lib.h>
#include <spl.h>
#include <spinlock.h>
#include <proc.h>
#include <current.h>
#include <mips/tlb.h>
#include <addrspace.h>
#include <vm.h>

/*
    So I'm using two different kinds of "pages tables".  One to keep track of which physical
    pages have been allocated, and one to keep track of virtual->physical address translations.
    
    ----- PHYSICAL MEMORY ALLOCATION -----
    
    The physical allocation page table is minimalistic, with page of physical memory being
    represented as a single bit.  0 if the page is available/unused, 1 if allocated.  This
    page table is a fixed size depenging on how much physical memory is available, and needs to
    be as large as is necessary to represent all of physical memory (minus the space needed for
    itself, which realistically will only be a few pages).
    
    ----- V->P MAPPING -----
    
    The virtual->physical page table is a bit more complicated.  The idea behind this table is
    that each virtual address will have a single entry that is 4 bytes (size of a pointer).
    This pointer will be the physical page that is assigned to that virtual page, with the low
    bits being used for special flags (see below).
    
    Since using a flat 4 bytes for all virtual pages would be excessive and a waste of space, the
    v->p page table is tiered.  There are one or more 'secondary' page tables which are as
    described above.  Secondary page tables are 1 page in size (4K).  Which means they can hold
    the page table entries for 1K v->p page mappings.... which effectively means one secondary
    page table is enough to address 4MB of virtual memory.
    
    There is then a 'master' page table which is structured the same, but each pointer represents
    a 4MB block, and points to the physical table that has the secondary page table for that block.
    
    So those "special flags" I mentioned....   kmalloc will request X number of pages, and when freed,
    it will expect all those pages to be deallocated, but it will not keep track of how many pages
    you're supposed to free.  It expects us to keep track of that and manage it.  So I keep track
    of this with flags!
    
    The VTBL_FIRST flag will be set if this page is the first page of an allocation.  When free is
    called, this flag MUST be set for the page given (otherwise they gave you a bad address).  The
    deallocator will then free THAT page, and every allocated page after it that does NOT have the
    flag set.  So it'll loop freeing pages until it hits an unallocated page, or a 'KVTBL_FIRST'
    page.
    
    
    Lastly, there is one page table for the kernel, and one page table for each user process.  Woo
    
*/

#define             NUMPAGES_PER_TABLE  (PAGE_SIZE / sizeof(paddr_t))

#define             STACKPAGECOUNT      16  // number of pages allocated for user program stacks (cannot really grow)
#define             HEAPPAGECOUNT       8   // number of pages allocated for the user heap (can grow as needed)

#define             DIRECTMEM_SIZE      (MIPS_KSEG1 - MIPS_KSEG0)   // number of bytes that can be directly accessed via KSEG0

static struct spinlock memSpinlock = SPINLOCK_INITIALIZER;

// Physical memory tracking!
static vaddr_t      physTblVaddr = 0;       // the vaddr of the physical memory allocation table
static paddr_t      physMemStart = 0;       // the start of usable physical memory (minus what is needed for this table)
static size_t       physPageCount = 0;      // total number of allocatable physical memory pages (minus this table)
static size_t       userNextPhysPage = 0;   // the next physical page index to examine when looking for user allocations
static size_t       kernNextPhysPage = 0;   // the next physical page index to examine when looking for kernel allocations
                                            
static vaddr_t      kernNextVirtualBlock = 0;   // the next virtual address to allocate for kernel memory

// Kernel virtual memory

static vaddr_t      vKernelMasterPT = 0;    // Virtual address of the Kernel's master page table


#define             PHYS_TBL        ((size_t*)physTblVaddr)
#define             NO_BLOCK_AVAIL  (~((size_t)0))




////////////////////////////////////

static inline int isPhysMemAvailable(size_t page)
{
    if(page >= physPageCount)
        return 0;
    
    return !( PHYS_TBL[page >> 5] & (1<<(page&31)) );
}

static inline void allocatePhysMemPage(size_t page)
{
    KASSERT( isPhysMemAvailable(page) );
    
    PHYS_TBL[page >> 5] |= 1<<(page&31);
}

static inline void freePhysMemPage(size_t page)
{
    KASSERT( page < physPageCount );
    KASSERT( !isPhysMemAvailable(page) );
    
    PHYS_TBL[page >> 5] &= ~(1<<(page&31));
}

// returns 0 if the pages are not available
//  returns 1 if they're available but non-contiguous
//  returns 2 if available and contiguous
static int checkPhysMemPagesAvailable(size_t* firstpage, int npages)
{
    int contiguous = 1;
    size_t startpage = *firstpage;
    size_t pg = startpage;
    int found = 0;
    int wrapped = 0;
    while(1)
    {
        if( isPhysMemAvailable(pg) )
        {
            ++found;
            if(found == 1)
            {
                *firstpage = pg;
                contiguous = 1;
            }
            if(found >= npages)     break;
        }
        else
            contiguous = 0;
             
        ++pg;
        if(pg >= physPageCount)
        {
            wrapped = 1;
            pg = 0;
            contiguous = 0;
        }
        
        if(wrapped && pg == startpage)
            break;
    }
    
    if(found >= npages)     // we found enough!
        return contiguous + 1;
    else
        return 0;
}

static paddr_t* allocateNewSubPageTable()
{
    // we are only allocating one page, so just walk through the page table looking for
    //   any slot.  Note this MUST be placed in direct mem
    int ptblsize = (int)((physPageCount + 31) / 32);
    int i;
    for(i = 0; PHYS_TBL[i] == 0xFFFFFFFF; ++i)
    {
        if(i >= ptblsize)       return 0;       // no available pages
    }
    
    int j = 0;
    for(j = 0; PHYS_TBL[i] & (1<<j); ++j)   {}
    
    size_t page = (i * 32) + j;
    paddr_t addr = (page * PAGE_SIZE) + physMemStart;
    
    if(addr >= DIRECTMEM_SIZE)  return 0;       // no available space in direct mem
    
    // take this page!
    PHYS_TBL[i] |= (1<<j);
    
    paddr_t* ptr = (paddr_t*)(PADDR_TO_KVADDR(addr));
    for(i = 0; i < (PAGE_SIZE/4); ++i)
        ptr[i] = 0;
    
    return ptr;
}

///   TODO --- LETS DO THIS
    *phys = bindPhysicalToVirtual( *phys, vad, npages, flags );

static vaddr_t findContiguousVirtualBlock( vaddr_t masterTable, vaddr_t nextAddr, int npages, int user )
{
    vaddr_t addrOffset = (user ? MIPS_KUSEG : MIPS_KSEG2);
    size_t pgStart = (user ? 1 : 0);
    size_t pgCount = (user ? MIPS_KUSEG_PAGECOUNT : MIPS_KSEG2_PAGECOUNT);
    
    paddr_t** tbl = (paddr_t**)(masterTable);
    size_t mn, sb;
    
    // Walk through virtual memory to find an open slot
    
    size_t page = (nextAddr / PAGE_SIZE);
    size_t firstpage = page;
    int run = 0;
    int wrapped = 0;
    
    while(1)
    {
        mn = (page+run) / NUMPAGES_PER_TABLE;
        sb = (page+run) % NUMPAGES_PER_TABLE;
        
        if(!tbl[mn])
        {
            tbl[mn] = (paddr_t*)allocateNewSubPageTable();
            if(!tbl[mn])
                return 0;
        }
        
        if(tbl[mn][sb])     // occupied
        {
            page += run + 1;
            run = 0;
        }
        else
        {
            ++run;
            if(run >= npages)
                break;          // found it!
        }
        
        if(wrapped && (page+run) > firstpage)       // searched entire space, no available spot
            return 0;
        if(page+run >= pgCount)
        {
            wrapped = 1;
            page = pgStart;
            run = 0;
        }
    }
    
    // only exited the loop if we found a block.  That block is in 'page'
    return (vaddr_t)( (page*PAGE_SIZE) + addrOffset );
}

// actually do the physical memory allocation!!!
static vaddr_t doPhysicalPageAllocation( int npages, int user, vaddr_t masterPageTable, vaddr_t* nextVaddr, int flags )
{
    if(npages <= 0)
        return 0;
    
    vmLock();
    
    // first, find a contiguous block of virtual memory at which to put this allocation
    vaddr_t vad = findContiguousVirtualBlock( masterPageTable, *nextVaddr, npages, user );
    if(!vad)
    {
        vmUnlock();
        return 0;
    }
    
    // next, see if we have 'npages' number of physical pages available
    size_t* phys = (user ? &userNextPhysPage : &kernNextPhysPage);
    int avail = checkPhysMemPagesAvailable( phys, npages );
    if(!avail)
    {
        vmUnlock();
        return 0;
    }
    
    // this can be direct memory if this is not a user allocation, if all blocks are contiguous,
    //    and if all blocks are in direct mem
    int isdirect = !user && (avail == 2) &&
                &&  ( (*phys + npages) * PAGE_SIZE) + physMemStart ) <= DIRECTMEM_SIZE;
                
    // if direct memory, we can use a direct vaddr
    if(direct)
        vad = PADDR_TO_KVADDR( (*phys * PAGE_SIZE) + physMemStart );
    
    *phys = bindPhysicalToVirtual( *phys, vad, npages, flags );
    vmUnlock();
    
    return vad;
}

////////////////////////////////////
////////////////////////////////////
////////////////////////////////////

void
vm_bootstrap(void)
{
    paddr_t lomem, himem;
    size_t ptblsize;
    
    //  This should only be called once at startup
    KASSERT( vKernelMasterPT == 0 );
    
    //  Get how much RAM we have to work with
    ram_getsize( &lomem, &himem );
    KASSERT( (lomem & PAGE_FRAME) == lomem );
    KASSERT( (himem & PAGE_FRAME) == himem );
    
    physPageCount = (himem - lomem) / PAGE_SIZE;
    KASSERT( physPageCount > 1 );
    
    // how many pages does our phys page table need to be?
    ptblsize = (physPageCount + 7) / 8;                 // number of bytes we need
    ptblsize = (ptblsize + PAGE_SIZE - 1) / PAGE_SIZE;  // number of pages we need
    ++ptblsize;             // add an extra page for the kernel master page table
    
    KASSERT( ptblsize > 1 );
    KASSERT( ptblsize < physPageCount );
    
    // All of the phys page table NEEDS to be in direct memory -- fuggit, I don't want to swap
    //   and maintain page tables for this
    physMemStart = lomem + (ptblsize * PAGE_SIZE);
    KASSERT( physMemStart <= DIRECTMEM_SIZE );
    
    physPageCount -= ptblsize;
    physTblVaddr = PADDR_TO_KVADDR(lomem);
    
    ///////////////////////
    //  Now that the phys allocation table is reserved, zero it out so all pages are available
	bzero(physTblVaddr, ptblsize * PAGE_SIZE);
    
    // I cheated a bit and took an extra page.  Make that page our kernel master page table
    vKernelMasterPT = PADDR_TO_KVADDR(physMemStart - PAGE_SIZE);
    
    /////////////////////
    //  lastly, set up next page counters
    kernNextPhysPage = 0;
    userNextPhysPage = (physPageCount / 8);    // kind of arbitrary
    
    kernNextVirtualBlock = MIPS_KSEG2;
}

/* Allocate/free some kernel-space virtual pages */
vaddr_t 
alloc_kpages(int npages)
{
    
    //  TODO REPLACE ALL THIS
    spinlock_acquire( &physTblSpinlock );
    
    size_t nextstart = kernNextPhysPage;
    int avail = arePhysMemPagesAvailable( &nextstart, npages, 0 );
    
    if(!avail)
    {
        spinlock_release( &physTblSpinlock );
    }
    
//static int arePhysMemPagesAvailable(size_t* firstpage, int npages, int growdown)

    
    size_t pg = allocatePhysMemBlock(kernNextPhysPage, npages);
    if(pg == NO_BLOCK_AVAIL)
    {
        spinlock_release( &physTblSpinlock );
        return 0;
    }
    
    kernNextPhysPage = pg + npages;
    if(kernNextPhysPage >= physPageCount)
        kernNextPhysPage = 0;
    
    spinlock_release( &physTblSpinlock );
    
    paddr_t paddr = (pg * PAGE_SIZE) + physMemStart;
    vaddr_t vaddr = PADDR_TO_KVADDR( paddr );
    
    
    
    //  assume this is in direct mem, because everything is in direct mem
    KASSERT( (pg + npages) <= DIRECTMEM_PAGES );
    
    return PADDR_TO_KVADDR((pg * PAGE_SIZE) + physMemStart);
}

void 
free_kpages(vaddr_t addr)
{
}

void
vm_tlbshootdown_all(void)
{
    panic("TLB Shootdowns are not supported!");
}

void
vm_tlbshootdown(const struct tlbshootdown *ts)
{
    (void)ts;
    panic("TLB Shootdowns are not supported!");
}

int
vm_fault(int faulttype, vaddr_t faultaddress)
{
}

struct addrspace *
as_create(void)
{
}

void
as_destroy(struct addrspace *as)
{
}

void
as_activate(void)
{
}

void
as_deactivate(void)
{
}

int
as_define_region(struct addrspace *as, vaddr_t vaddr, size_t sz,
		 int readable, int writeable, int executable)
{
}

int
as_prepare_load(struct addrspace *as)
{
}

int
as_complete_load(struct addrspace *as)
{
}

int
as_define_stack(struct addrspace *as, vaddr_t *stackptr)
{
}

int
as_copy(struct addrspace *old, struct addrspace **ret)
{
}
