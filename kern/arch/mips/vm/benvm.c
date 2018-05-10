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

#define             STACKPAGECOUNT      16  // number of pages allocated for user program stacks (cannot really grow)
#define             HEAPPAGECOUNT       8   // number of pages allocated for the user heap (can grow as needed)

#define             DIRECTMEM_SIZE      (MIPS_KSEG1 - MIPS_KSEG0)   // number of bytes that can be directly accessed via KSEG0
#define             DIRECTMEM_PAGES     (DIRECTMEM_SIZE / PAGE_SIZE)

// Physical memory tracking!
static vaddr_t      physTblVaddr = 0;       // the vaddr of the physical memory allocation table
static paddr_t      physMemStart = 0;       // the start of usable physical memory (minus what is needed for this table)
static size_t       physPageCount = 0;      // total number of allocatable physical memory pages (minus this table)
static size_t       userNextPhysPage = 0;   // the next physical page index to examine when looking for user allocations
                                            //    this actually starts at the top and grows down
static size_t       kernNextPhysPage = 0;   // the next physical page index to examine when looking for kernel allocations
                                            //    this actually starts at the bottom and grows up
                                            //      This helps keep kernel memory in low physical addresses so it can be
                                            //      accessed via KSEG0 (fewer page faults)

// Kernel virtual memory

static vaddr_t      vKernelMasterPT = 0;    // Virtual address of the Kernel's master page table


#define             PHYS_TBL        ((unsigned long*)physTblVaddr)




////////////////////////////////////

static inline int isPhysMemAvailable(size_t page)
{
    return !( PHYS_TBL[page >> 3] & (1<<(page&7)) );
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
    userNextPhysPage = (physPageCount - 1);
}

/* Allocate/free some kernel-space virtual pages */
vaddr_t 
alloc_kpages(int npages)
{
    if(npages <= 0)
        return 0;
    
    // 
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
