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

struct TestInfo
{
    int numthreads;
    int numloops;
};

static
void
init_sem(void)
{
	if (tsem==NULL) {
		tsem = sem_create("tsem", 0);
		if (tsem == NULL) {
			panic("threadfuntest: sem_create failed\n");
		}
	}
}

static
void
destroy_sem(void)
{
    if(tsem != NULL)
    {
        sem_destroy(tsem);
        tsem = NULL;
    }
}

/*
 * The idea with this is that you should see
 *
 *   01234567 <pause> 01234567
 *
 * (possibly with the numbers in different orders)
 *
 * The delay loop is supposed to be long enough that it should be clear
 * if either timeslicing or the scheduler is not working right.
 */
static
void
testthread(void *vinfo, unsigned long x)
{
    struct TestInfo* info = vinfo;
	int ch = '0' + x;
    int i, j;
  
    for(i = 0; i < numloops; ++i)
    {
        putch(ch);
        
        for(j = 0; j < 10000; ++j)
            ;
    }

	V(tsem);
}

static
void
runthreads(struct TestInfo* info)
{
	char name[16];
	int i, result;

	for (i=0; i<numthreads; i++) {
		snprintf(name, sizeof(name), "threadfuntest%d", i);
		result = thread_fork(name, NULL,
				     testthread,
				     info, i);
		if (result) {
			panic("threadfuntest: thread_fork failed %s)\n", 
			      strerror(result));
		}
	}

	for (i=0; i<numthreads; i++) {
		P(tsem);
	}
}


int
threadfuntest(int nargs, char **args)
{
    struct TestInfo info;
    
    if(nargs < 3) {
        kprintf("Usage:  tfun <number_of_threads> <number_of_loops>\n");
    }
    if(nargs < 2) {
        kprintf("Number of threads parameter missing, defaulting to 5 threads.\n");
        info.numthreads = 5;
    } else {
        info.numthreads = atoi(args[1]);
    }
    if(nargs < 3) {
        kprintf("Number of loops parameter missing, defaulting to 8 loop iterations.\n");
        info.numloops = 8;
    } else {
        info.numloops = atoi(args[2]);
    }
    
	init_sem();
	kprintf("Starting thread fun test with %d threads...\n", numthreads);
	runthreads(&info);
    
    destroy_sem();
	kprintf("\nThread test done.\n");

	return 0;
}
