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

    To see if a page of memory is allocated, I have a 'pageUse' table.  This table is basically
    a big array where each individual bit indicates if a page is available (0) or allocated (1).
    
    Example:  If there are 1000 pages of physical memory, the pageUse table would need to be
    ceil(1000/8) = 125 bytes wide:
        pageUse[0] & 0x01  =   state of physical page 0
        pageUse[0] & 0x02  =   state of physical page 1
            ...
        pageUse[0] & 0x80  =   state of physical page 7
        pageUse[1] & 0x01  =   state of physical page 8
            ...
            etc
            
    (though these will generally be accessed via 4-byte unsigned longs rather than by individual bytes)
    
    Details of how each page is allocated (like which proc own it, whether it's writable, etc) is not
    stored in this table, but instead is stored in the addrspace struct which is owned by each proc.
*/

/*
    We have ANOTHER 'pageUse' table called 'kvPageUse' to indicate which page of VIRTUAL
    memory is occupied in the MIPS_KSEG2 segment.
*/

/*
    Let's talk about kmalloc!
    
    Or, rather, allocating pages of memory for the kernel.  (alloc_kpages)
    
    We will designate 1 full page to host the addrspace_block's for kernel allocation.
    The page is filled with individual addrspace_block entries.  Each entry is for individual
    calls to alloc_kpages.
    
    Since more pages may be needed (if the number of alloc_kpages calls exceeds the amount
    that can be recorded on a single page), the LAST addrspace_block on the page is reserved
    to refer to the NEXT page
    
    I call these the 'kMemBlock' pages
*/

//static struct spinlock stealmem_lock = SPINLOCK_INITIALIZER;

//  stuff for the pageUse table
static vaddr_t      pageUseVAddr = 0;           // virtual address of the 'pageUse' table
static paddr_t      pageUsePAddr = 0;           // physical address
static size_t       pageUsePageCount = 0;       // number of pages used by the 'pageUse' table
static size_t       availablePages = 0;         // number of available pages of physical memory at startup (doesn't change as things are allocated)
static paddr_t      physicalMemAddr = 0;        // address of start of [unstolen] physical memory

//  stuff for the kvPageUse table
static vaddr_t      kvPageUseVAddr = 0;         // virtual address of the 'kvPageUse' table
static paddr_t      kvPageUsePAddr = 0;         // physical address
#define             kvPageUseCount   (size_t)((size_t)(0 - MIPS_KSEG2) / (PAGE_SIZE * 8))

//  stuff for kMemBlock
static vaddr_t      kMemBlockVAddr = 0;         // virtual address of the first kMemBlock page
static paddr_t      kMemBlockPAddr = 0;         // physical address


void
vm_bootstrap(void)
{
    KASSERT( sizeof(
    KASSERT( availablePages == 0 );     // should only be called once!
    
    paddr_t ramlo, ramhi;
 
    // figure out how much physical memory we have to work with, and use that
    // to determine the size of the 'pageUse' table
    ram_getsize(&ramlo, &ramhi);
    
    KASSERT( (ramlo & PAGE_FRAME) == ramlo );
    KASSERT( (ramhi & PAGE_FRAME) == ramhi );
    
    pageUsePAddr = ramlo;
    availablePages = (ramhi - ramlo) / PAGE_SIZE;
    
    KASSERT( availablePages > 0 );
    
    pageUsePageCount = (availablePages + (PAGE_SIZE * 8) - 1) / (PAGE_SIZE * 8);
    
    KASSERT( pageUsePageCount >= 1 );
    KASSERT( pageUsePageCount < availablePages );
    
    availablePages -= pageUsePageCount;
    
    physicalMemAddr = pageUsePAddr + (pageUsePageCount * PAGE_SIZE);
    
    //  put this at the very end of addressible memory, since that is Kernel-accessible
    //    and is TLB managed.
    pageUseVAddr = (vaddr_t)0 - (pageUsePageCount * PAGE_SIZE);
    KASSERT( pageUseVAddr >= MIPS_KSEG2 );
    
    // Now that we have our pages for our pageUse table, we need to zero it to indicate
    //   that all pages are available!
    bzero( (void*)(pageUseVAddr), pageUsePageCount * PAGE_SIZE );
}

/* Allocate/free some kernel-space virtual pages */
vaddr_t 
alloc_kpages(int npages)
{
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
