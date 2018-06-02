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

#define             NUMPAGES_PER_TABLE      (PAGE_SIZE / sizeof(paddr_t))
#define             MAX_ADDRESSABLE_PAGES   (0x10000)

#define             STACKPAGECOUNT      16  // number of pages allocated for user program stacks (cannot really grow)
//#define             HEAPPAGECOUNT       8   // number of pages allocated for the user heap (can grow as needed)

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


static inline int vmLock()
{
    spinlock_acquire(&memSpinlock);
}

static inline int vmUnlock()
{
    spinlock_release(&memSpinlock);
}


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

static void addEntryToPageTable( vaddr_t tableAddr, vaddr_t vaddr, paddr_t paddr, int flags )
{
    paddr_t** tbl = (paddr_t**)(tableAddr);
    size_t mn, sb;
    
    mn = (vaddr / PAGE_SIZE) / NUMPAGES_PER_TABLE;
    sb = (vaddr / PAGE_SIZE) % NUMPAGES_PER_TABLE;
    
    if(!tbl[mn])
    {
        tbl[mn] = allocateNewSubPageTable();
        KASSERT( tbl[mn] );                     // this probably shouldn't be a KASSERT since this is just an out of mem error, but whatever
    }
    
    tbl[mn][sb] = paddr | flags;
}

static size_t bindPhysicalToVirtual( vaddr_t masterTableAddr, size_t physPage, vaddr_t virtaddr, int npages, int flags )
{
    KASSERT(physPage < physPageCount);
    
    size_t forevercheck = physPage;
    flags |= VTBL_FIRST;
    
    while(npages > 0)
    {
        if(isPhysMemAvailable(physPage))
        {
            allocatePhysMemPage( physPage );
            addEntryToPageTable( masterTableAddr, virtaddr, ((physPage*PAGE_SIZE) + physMemStart), flags );
            --npages;
            virtaddr += PAGE_SIZE;
            flags &= ~VTBL_FIRST;
        }
    
        ++physPage;
        if(physPage >= physPageCount)
            physPage = 0;
        
        KASSERT( forevercheck != physPage || npages <= 0 );
    }
    
    return physPage;
}

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
static vaddr_t doPhysicalPageAllocation( int npages, int user, vaddr_t masterPageTable, vaddr_t* nextVaddr, int flags, int forceVaddr )
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
    if(forceVaddr && *nextVaddr != vad)
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
    int isdirect = !user && (avail == 2)
                &&  ( ( (*phys + npages) * PAGE_SIZE) + physMemStart ) <= DIRECTMEM_SIZE;
                
    // if direct memory, we can use a direct vaddr
    if(isdirect)
        vad = PADDR_TO_KVADDR( (*phys * PAGE_SIZE) + physMemStart );
    
    *phys = bindPhysicalToVirtual( masterPageTable, *phys, vad, npages, flags );
    vmUnlock();
    
    return vad;
}

static paddr_t getPhysicalAddrFromPageTable( vaddr_t pagetable, vaddr_t vaddr )
{
    size_t mn, sb;
    paddr_t** tbl = (paddr_t**)(pagetable);
    
    vaddr /= PAGE_SIZE;
    mn = vaddr / NUMPAGES_PER_TABLE;
    sb = vaddr % NUMPAGES_PER_TABLE;
    
    if(!tbl[mn])        return 0;
    return tbl[mn][sb] & PAGE_FRAME;
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
	bzero((void*)physTblVaddr, ptblsize * PAGE_SIZE);
    
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
    return doPhysicalPageAllocation( npages, 0, vKernelMasterPT, &kernNextPhysPage, VTBL_READ | VTBL_WRITE | VTBL_EXEC, 0 );
}

void 
free_kpages(vaddr_t addr)
{
    KASSERT( (addr & PAGE_FRAME) == addr );
    
    paddr_t** tbl = (paddr_t**)vKernelMasterPT;
    size_t page = (addr / PAGE_SIZE);
    size_t mn, sb;
    size_t physpage;
    
    int first = 0;
    
    vmLock();
    while(page < MAX_ADDRESSABLE_PAGES)
    {
        mn = page / NUMPAGES_PER_TABLE;
        sb = page % NUMPAGES_PER_TABLE;
        
        if(tbl[mn] && (tbl[mn][sb] & VTBL_FIRST))
        {
            if(!first)
                break;          // this is a different allocated block.  We're done
        }
        else if(first)
        {
            KASSERT(0);         // They gave us a bad address to free!
        }
        first = 0;
        
        // if we made it here, if this page is allocated we need to free it
        if(!tbl[mn])            break;  // not allocated
        if(!tbl[mn][sb])        break;  // not allocated
        
        // If we made it here, it IS allocated and we need to free it
        physpage = (tbl[mn][sb] - physMemStart) / PAGE_SIZE;
        freePhysMemPage(physpage);
        tbl[mn][sb] = 0;
        page++;
        
        //  TODO - maybe remove this page from the TLB??
    }
    vmUnlock();
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
    size_t vpage = faultaddress / PAGE_SIZE;
    size_t mn, sb;
    paddr_t phys = 0;
	struct addrspace *as;
    uint32_t elo, ehi;
    int spl;
    
    if(faulttype == VM_FAULT_READONLY)
        return EFAULT;
    /*
    TODO - users can only access user mem and kernel can only
    access kernel mem?  Not sure that's right.
    if(!curproc && faultaddress < MIPS_KSEG0)
        return EFAULT;
    if(curproc && faultaddress >= MIPS_KSEG0)
        return EFAULT;
    */
    
    paddr_t** tbl = (paddr_t**)vKernelMasterPT;
    
    int allowwriting = 1;
    if(curproc)
    {
        as = curproc_getas();
        KASSERT(as != 0);
        
        tbl = (paddr_t**)as->vPageTable;
        allowwriting = as->isLoading;
    }
    
    mn = vpage / NUMPAGES_PER_TABLE;
    sb = vpage % NUMPAGES_PER_TABLE;
    phys = 0;
    vmLock();
    if(tbl[mn])
    {
        phys = tbl[mn][sb];
    }
    vmUnlock();
    
    if(phys & VTBL_WRITE)
        allowwriting = 1;
    
    // if we are writing, the page we're accessing must be writable
    if( (faulttype == VM_FAULT_WRITE) && !allowwriting )
        return EFAULT;
    
    // OK!  All is good to put this freaking page in!
    ehi = faultaddress & PAGE_FRAME;
    elo = (phys & PAGE_FRAME) | TLBLO_VALID;
    if(allowwriting)
        elo |= TLBLO_DIRTY;
    
	spl = splhigh();
    tlb_random(ehi, elo);
	splx(spl);
    
    return 0;
}

static void as_initializeSegment( struct addrspace_segment* seg )
{
    seg->vAddr = 0;
    seg->nPages = 0;
    seg->refCount = 1;
    seg->flags = VTBL_READ | VTBL_WRITE;
    spinlock_init( &seg->refSpin );
}

static void as_freeVirtBlock( vaddr_t pagetable, struct addrspace_segment* seg, int should_free_phys_mem )
{
    paddr_t** tbl = (paddr_t**)pagetable;
    
    size_t page = seg->vAddr / PAGE_SIZE;
    size_t mn, sb;
    vmLock();
    for(int i = 0; i < seg->nPages; ++i)
    {
        mn = (page+i) / NUMPAGES_PER_TABLE;
        sb = (page+i) % NUMPAGES_PER_TABLE;
        
        KASSERT(tbl[mn]);
        KASSERT(tbl[mn][sb]);
        
        if(should_free_phys_mem)
            freePhysMemPage( (tbl[mn][sb] - physMemStart) / PAGE_SIZE );
        tbl[mn][sb] = 0;
    }
    vmUnlock();
}

static void as_destroySegment( vaddr_t pagetable, struct addrspace_segment* seg, int should_kfree )
{
    size_t refcount;
    
    spinlock_acquire( &seg->refSpin );
    seg->refCount--;
    refcount = seg->refCount;
    spinlock_release( &seg->refSpin );
    
    as_freeVirtBlock( pagetable, seg, refcount == 0 );
    
    if(refcount == 0)
    {
        spinlock_cleanup( &seg->refSpin );
        if(should_kfree)
            kfree(seg);
    }
}

static int deepcopy_as_segment( vaddr_t dstpagetable, struct addrspace_segment* dst, vaddr_t srcpagetable, struct addrspace_segment* src )
{
    if(src->nPages < 1)
        return 1;
    
    vaddr_t tmp = src->vAddr;
    vaddr_t vaddr = doPhysicalPageAllocation( src->nPages, 1, dstpagetable, &tmp, src->flags, 1 );
    if(vaddr != src->vAddr)
        return 1;
    
    dst->vAddr = vaddr;
    dst->nPages = src->nPages;
    dst->refCount = 1;
    dst->flags = src->flags;
    
    vmLock();
    int spl = splhigh();
    
    uint32_t dstlo, srclo;
    uint32_t dsthi = MIPS_KSEG2;
    uint32_t srchi = MIPS_KSEG2 + PAGE_SIZE;
    
    for(int page = 0; page < src->nPages; page++)
    {
        tmp = vaddr + (page * PAGE_SIZE);
        srclo = getPhysicalAddrFromPageTable( srcpagetable, tmp ) | TLBLO_VALID;
        dstlo = getPhysicalAddrFromPageTable( dstpagetable, tmp ) | TLBLO_VALID | TLBLO_DIRTY;
        
        tlb_write(dsthi, dstlo, 0);
        tlb_write(srchi, srclo, 1);
        
        memmove((void *)dsthi, (const void *)srchi, PAGE_SIZE);
    }
    
    tlb_write(TLBHI_INVALID(0), TLBLO_INVALID(), 0);
    tlb_write(TLBHI_INVALID(1), TLBLO_INVALID(), 1);
    
    splx(spl);
    vmUnlock();
    
    return 0;
}

static int shallowcopy_as_segment( vaddr_t dstpagetable, struct addrspace_segment** dst, vaddr_t srcpagetable, struct addrspace_segment* src )
{
    struct addrspace_segment* seg;
    
    if(src->flags & VTBL_WRITE)
    {
        seg = kmalloc( sizeof(struct addrspace_segment) );
        if(!seg) return 1;
        
        as_initializeSegment(seg);
        
        if(deepcopy_as_segment(dstpagetable, seg, srcpagetable, src))
        {
            as_destroySegment(dstpagetable, seg, 1);
            return 1;
        }
    }
    else
    {
        spinlock_acquire(&src->refSpin);
        src->refCount++;
        spinlock_release(&src->refSpin);
        seg = src;
    }
    
    *dst = seg;
    return 0;
}

struct addrspace *
as_create(void)
{
    struct addrspace* as = kmalloc( sizeof(struct addrspace) );
    if(!as)
        return 0;
    
    as->segments = kmalloc( sizeof(struct addrspace_segment*) * 4 );
    if(!as->segments) {
        kfree(as);
        return 0;
    }
    
    vmLock();
    as->vPageTable = (vaddr_t)(allocateNewSubPageTable());
    vmUnlock();
    if(!as->vPageTable) {
        kfree(as->segments);
        kfree(as);
        return 0;
    }
    
    // all memory allocated!
    as_initializeSegment(&as->stackSeg);
    as->segCount = 0;
    as->segSize = 4;
    as->nextVAddr = 10 * PAGE_SIZE;
    as->isLoading = 0;
    
    return as;
}

void
as_destroy(struct addrspace *as)
{
    KASSERT(as != 0);
    size_t i;
    as_destroySegment( as->vPageTable, &as->stackSeg, 0 );
    for(i = 0; i < as->segCount; ++i)
    {
        as_destroySegment( as->vPageTable, as->segments[i], 1 );
    }
    
    kfree(as->segments);
    kfree(as);
}

int
as_copy(struct addrspace *old, struct addrspace **ret)
{
    size_t i;
    KASSERT(old != 0);
    KASSERT(ret != 0);
    
    struct addrspace* as = kmalloc(sizeof(struct addrspace));
    if(!as)
        return ENOMEM;
    
    as->segCount = 0;
    as->nextVAddr = old->nextVAddr;
    as->segSize = old->segCount;
    as->segments = kmalloc( sizeof(struct addrspace_segment*) * old->segCount );
    if(!as->segments) {
        kfree(as);
        return ENOMEM;
    }
    
    as_initializeSegment(&as->stackSeg);
    vmLock();
    as->vPageTable = (vaddr_t)(allocateNewSubPageTable());
    vmUnlock();
    if(!as->vPageTable)
        goto failure;
    
    if(deepcopy_as_segment(as->vPageTable, &as->stackSeg, old->vPageTable, &old->stackSeg))
        goto failure;
    
    for(i = 0; i < old->segCount; ++i)
    {
        if(shallowcopy_as_segment(as->vPageTable, &as->segments[i], old->vPageTable, old->segments[i]))
            goto failure;
        as->segCount++;
    }
    
    *ret = as;
    return 0;
    
failure:
    as_destroy(as);
    return ENOMEM;
}

static void clear_tlb(void)
{
	int i, spl;
	struct addrspace *as;

	as = curproc_getas();
	if (as == NULL) {
		return;
	}

	/* Disable interrupts on this CPU while frobbing the TLB. */
	spl = splhigh();

	for (i=0; i<NUM_TLB; i++) {
		tlb_write(TLBHI_INVALID(i), TLBLO_INVALID(), i);
	}

	splx(spl);
}

void
as_activate(void)
{
    clear_tlb();
}

void
as_deactivate(void)
{
    // nothing to do here
}

int
as_define_region(struct addrspace *as, vaddr_t vaddr, size_t sz,
		 int readable, int writeable, int executable)
{
    KASSERT((vaddr & PAGE_FRAME) == vaddr);
    KASSERT(as != 0);
    KASSERT(sz > 0);
    
    if(as->segCount == as->segSize)
    {
        size_t newsize = as->segSize*2;
        struct addrspace_segment** newbuf = kmalloc(sizeof(struct addrspace_segment*) * newsize);
        if(!newbuf)
            return ENOMEM;
        
        for(size_t i = 0; i < as->segCount; ++i)
            newbuf[i] = as->segments[i];
        
        kfree(as->segments);
        as->segments = newbuf;
        as->segSize = newsize;
    }
    
    struct addrspace_segment* seg = kmalloc(sizeof(struct addrspace_segment));
    if(!seg)
        return ENOMEM;
    
    as_initializeSegment(seg);
    seg->vAddr = vaddr;
    seg->nPages = (sz + PAGE_SIZE - 1) / PAGE_SIZE;
    seg->flags = 0;
    if(readable)    seg->flags |= VTBL_READ;
    if(writeable)   seg->flags |= VTBL_WRITE;
    if(executable)  seg->flags |= VTBL_EXEC;
    
    vaddr_t tmp = vaddr;
    vaddr_t result = doPhysicalPageAllocation( seg->nPages, 1, as->vPageTable, &tmp, seg->flags, 1 );
    if(result != vaddr)
    {
        as_destroySegment( as->vPageTable, seg, 1 );
        return ENOMEM;
    }
    
    as->segments[as->segCount] = seg;
    as->segCount++;
    return 0;
}

int
as_prepare_load(struct addrspace *as)
{
    KASSERT(as != 0);
    clear_tlb();
    as->isLoading = 1;
    return 0;
}

int
as_complete_load(struct addrspace *as)
{
    KASSERT(as != 0);
    clear_tlb();
    as->isLoading = 0;
    return 0;
}

int
as_define_stack(struct addrspace *as, vaddr_t *stackptr)
{
    KASSERT(as);
    KASSERT(as->stackSeg.nPages == 0);
    
    as->stackSeg.flags = VTBL_READ | VTBL_WRITE;
    as->stackSeg.vAddr = doPhysicalPageAllocation( STACKPAGECOUNT, 1, as->vPageTable, &as->nextVAddr, as->stackSeg.flags, 0 );
    if(!as->stackSeg.vAddr)
        return ENOMEM;
    as->stackSeg.nPages = STACKPAGECOUNT;
    
    *stackptr = as->stackSeg.vAddr + (as->stackSeg.nPages * PAGE_SIZE);
    return 0;
}

