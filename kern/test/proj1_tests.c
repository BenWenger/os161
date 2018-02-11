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

/*
 * Thread test code.
 */
#include <types.h>
#include <lib.h>
#include <thread.h>
#include <synch.h>
#include <test.h>


static struct semaphore *tsem = NULL;
static struct spinlock* spinlock = NULL;
static struct lock* lock = NULL;
static volatile unsigned long test_ctr = 0;

static
void
init_sem(void)
{
	if (tsem==NULL) {
		tsem = sem_create("tsem", 0);
		if (tsem == NULL) {
			panic("project 1 test: sem_create failed\n");
		}
	}
}

static
void
init_spinlock(void)
{
	if (spinlock==NULL) {
		spinlock = kmalloc(sizeof(struct spinlock));
		if (spinlock == NULL) {
			panic("project 1 test: kmalloc failed when creating spin lock\n");
		}
        spinlock_init(spinlock);
	}
}

static
void
init_lock(void)
{
	if (lock==NULL) {
		lock = lock_create("Proj1 Lock");
		if (lock == NULL) {
			panic("project 1 test: lock_create failed\n");
		}
	}
}

static
void
destroy_all(void)
{
    if(tsem != NULL)
    {
        sem_destroy(tsem);
        tsem = NULL;
    }
    if(spinlock != NULL)
    {
        spinlock_cleanup(spinlock);
        kfree(spinlock);
        spinlock = NULL;
    }
    if(lock != NULL)
    {
        lock_destroy(lock);
        lock = NULL;
    }
}

static
void
testthread(void *junk, unsigned long loops)
{
    unsigned long i;
    (void)(junk);
  
    for(i = 0; i < loops; ++i)
    {
        if(spinlock)        spinlock_acquire(spinlock);
        if(lock)            lock_acquire(lock);
        
        test_ctr++;
        
        if(spinlock)        spinlock_release(spinlock);
        if(lock)            lock_release(lock);
    }

	V(tsem);
}

static
void
runthreads(int numthreads, int numloops)
{
	char name[16];
	int i, result;

	for (i=0; i < numthreads; i++) {
		snprintf(name, sizeof(name), "proj1test%d", i);
		result = thread_fork(name, NULL,
				     testthread,
				     NULL, numloops);
		if (result) {
			panic("proj1test: thread_fork failed %s)\n", 
			      strerror(result));
		}
	}

	for (i=0; i<info->numthreads; i++) {
		P(tsem);
	}
}

int doProj1Test(int nargs, char** args)
{
    int numthreads;
    int numloops;
    unsigned long expected;
    
    init_sem();
    
    if(nargs < 3) {
        kprintf("Usage:  <testname> <number_of_threads> <number_of_loops>\n");
    }
    if(nargs < 2) {
        kprintf("Number of threads parameter missing, defaulting to 5 threads.\n");
        numthreads = 5;
    } else {
        numthreads = atoi(args[1]);
    }
    if(nargs < 3) {
        kprintf("Number of loops parameter missing, defaulting to 10000 loop iterations.\n");
        numloops = 10000;
    } else {
        numloops = atoi(args[2]);
    }
    
	kprintf("Starting thread fun test with %d threads...\n", numthreads);
    test_ctr = 0;
    expected = (unsigned long)(numloops) * numthreads;
    
	runthreads(numthreads, numloops);
    
    destroy_all();
    
	kprintf("\nThread test done.\nExpected counter value:  %ul\nActual counter value:  %ul\n\n", expected, test_ctr);
    
    if(test_ctr == expected) {
        kprintf("TEST PASSED\n\n");
    } else {
        kprintf("TEST FAILED\n\n");
    }

	return 0;
}

int
unsafethreadcounter(int nargs, char **args)
{
    return doProj1Test(nargs, args);
}

int
lockthreadcounter(int nargs, char **args)
{
    init_lock();
    return doProj1Test(nargs, args);
}

int
spinlockthreadcounter(int nargs, char **args)
{
    init_spinlock();
    return doProj1Test(nargs, args);
}