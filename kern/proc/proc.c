/*
 * Copyright (c) 2013
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
 * Process support.
 *
 * There is (intentionally) not much here; you will need to add stuff
 * and maybe change around what's already present.
 *
 * p_lock is intended to be held when manipulating the pointers in the
 * proc structure, not while doing any significant work with the
 * things they point to. Rearrange this (and/or change it to be a
 * regular lock) as needed.
 *
 * Unless you're implementing multithreaded user processes, the only
 * process that will have more than one thread is the kernel process.
 */

#include <types.h>
#include <proc.h>
#include <current.h>
#include <addrspace.h>
#include <vnode.h>
#include <vfs.h>
#include <synch.h>
#include <kern/fcntl.h> 
#include <kern/limits.h>

/*
 * The process for the kernel; this holds all the kernel-only threads.
 */
struct proc *kproc;

/*
 * Mechanism for making the kernel menu thread sleep while processes are running
 */
#ifdef UW
/* count of the number of processes, excluding kproc */
static unsigned int proc_count;
/* provides mutual exclusion for proc_count */
/* it would be better to use a lock here, but we use a semaphore because locks are not implemented in the base kernel */ 
static struct semaphore *proc_count_mutex;
/* used to signal the kernel menu thread when there are no processes */
struct semaphore *no_proc_sem;

#endif  // UW


/*********************************************

This is my proc table!

It's a resizable array of proc*s.  When a proc is created, it is assigned a pid and
inserted into this array.  When a proc is destroyed, the entry in this array **may persist**
a little longer, so that other processes can poll the state and exit code.

Specifically:
- A "parent" process can poll the state of any of its children. Therefore when a child
    proc exits, its table entry must persist.
- When a parent process exits, all of its children are iterated, and any children
    that have exited are removed from the table.  Any children still running have their
    parent_pid cleared.
- When ANY proc is destroyed, if its parent_pid is cleared (parent has exited already),
    or if the parent_pid is 1 (parent is kproc), the proc is immediately removed from the
    table.
- Only "parent" procs can waitpid() on their children. If a proc tries to waitpid() on
    a proc that isn't a direct child, waitpid() will fail.
    

One other thing to note about this table:  the index for the table is "pid - __PID_MIN",
so do not access the table with the pid directly.


A note about deadlock:
    Once you acquire proc_table_mutex, DO NOT ATTEMPT TO ACQUIRE ANY PROC'S LOCK.
It is OK to have both acquired at the same time AS LONG AS THE PROC'S LOCK IS ACQUIRED
FIRST.
    Don't acquire more than one proc's lock at a time.

***********************************************/

#define MAX_PTBL_SIZE       (__PID_MAX - PIX_MIN + 1)
#define START_PTBL_SIZE     20

static struct semaphore* proc_table_mutex;
static struct proc** proc_table;        // I probably could use the 'array' lib for this, but I looked at it and it's a pain
static unsigned proc_table_size;        //      and really, all I need is a simple realloc

static void
ptbl_create()
{
    KASSERT( proc_table_mutex == NULL );
    KASSERT( proc_table == NULL );
    
    proc_table_size = START_PTBL_SIZE;
    proc_table = kmalloc( sizeof(*proc_table) * proc_table_size );
    if(proc_table == NULL) {
        panic("could not create proc_table\n");
    }
    for(i = 0; i < proc_table_size; ++i)    proc_table[i] = NULL;
    
    proc_table_mutex = sem_create("proc_table_mutex",1);
    if (proc_table_mutex) {
        panic("could not create proc_table_mutex semaphore\n");
    }
}

static int
unsafe_ptbl_realloc()           // unsafe because it assumes proc_table_mutex is held
{
    unsigned i;
    unsigned newsize;
    struct proc** x;
    
    newsize = proc_table_size * 2;
    if(newsize > MAX_PTBL_SIZE)     newsize = MAX_PTBL_SIZE;
    if(newsize <= proc_table_size)  return 1;       // can't allocate any more
    
    x = kmalloc( sizeof(*x) * newsize );
    if(x == NULL)                   return 1;       // no more mem!!
    
    for(i = 0; i < proc_table_size; ++i)        x[i] = proc_table[i];
    for(i = proc_table_size; i < newsize; ++i)  x[i] = NULL;
    
    kfree(proc_table);
    proc_table = x;
    proc_table_size = newsize;
    return 0;
}

static int
ptbl_addproc(struct proc* proc)
{
    unsigned i;
    
    // if the table mutex hasn't been created yet -- this is kproc.  We don't want to add kproc
    //    just ignore this
    if(proc_table_mutex == NULL)
        return 0;
    
    P(proc_table_mutex);
    
    // find an empty slot
    for(i = 0; i < proc_table_size; ++i)
    {
        if(proc_table[i] == NULL)       break;  // slot i is available!
    }
    
    // if there was no empty slot, realloc to get a slot
    if(i == proc_table_size)
    {
        if(unsafe_ptbl_realloc())
        {
            V(proc_table_mutex);
            return 1;           // we couldn't realloc -- can't add this proc
        }
    }
    
    proc_table[i] = proc;
    proc->pid = (pid_t)(i + __PID_MIN);
    V(proc_table_mutex);
    return 0;
}

static void
ptbl_removeproc(struct proc* p)
{
    unsigned i;
    struct proc* sub;
    pid_t mypid;
    pid_t myparentpid;
    
    KASSERT(p != NULL);
    
    lock_acquire(p->lk);
    mypid = p->pid;
    myparentpid = p->parent_id;
    lock_release(p->lk);
    
    KASSERT(mypid >= __PID_MIN);
    KASSERT(mypid <= __PID_MAX);
    
    // Look for our children and clean them up / mark them to be cleaned up
    //   This is O(n) since I traverse the entire table looking for children.
    // A more optimized approach might be to have a proc keep track of its
    // children, but that'd be more work and I'm not sure it'd be much better.
    
    P(proc_table_mutex);
    for(i = 0; i < proc_table_size; ++i)        
    {
        if(proc_table[i] == NULL)       continue;   // this entry is already empty -- skip it
        sub = proc_table[i];
        
        V(proc_table_mutex);        // release proc table mutex
        lock_acquire(sub->lk);      // before acquiring proc mutex
        
        if(sub->parent_id == mypid)   // this entry is one of our children
        {
            if(sub->running == 0)   // the child has already exited
            {
                // the child has already exited.  Clean it up!
                lock_release(sub->lk);
                lock_destroy(sub->lk);
                cv_destroy(sub->cv);
                kfree(sub);
                P(proc_table_mutex);
                proc_table[i] = NULL;
            }
            else
            {
                // The child is still running, just mark the parent as dead
                sub->parent_id = 0;
                lock_release(sub->lk);
                P(proc_table_mutex);
            }
        }
    }
    V(proc_table_mutex);
    
    // now... remove US!
    if(myparentpid == 0 || myparentpid == 1)        // if our parent has exited, or our parent is kproc
    {
        lock_destroy(p->lk);
        cv_destroy(p->cv);
        kfree(sub);
        P(proc_table_mutex);
        proc_table[mypid - __PID_MIN] = NULL;
        V(proc_table_mutex);
    }
    else
    {
        // our parent is still around, mark us as no longer running,
        //   and broadcast so we wake up any waiting procs
        lock_acquire(p->lk);
        p->running = 0;
        cv_broadcase(p->cv, p->lk);
        lock_release(p->lk);
    }
}

/* Set the exit code for a process */
void proc_setexitcode(struct proc* proc, int exitcode)
{
    lock_acquire(proc->lk);
    proc->exitcode = exitcode;
    lock_release(proc->lk);
}

int proc_waitpid(pid_t waitpid, int* exitcode)
{
    struct proc* targetproc;
    pid_t mypid;
    
    if(waitpid < __PID_MIN)     return ESRCH;
    if(waitpid > __PID_MAX)     return ESRCH;
    
    // get my pid
    mypid = curproc->pid;
    
    // get the target process
    targetproc = NULL;
    P(proc_table_mutex);
    if(waitpid - __PID_MIN < proc_table_size)
        targetproc = proc_table[waitpid - __PID_MIN];
    V(proc_table_mutex);
    
    if(targetproc == NULL)      return ESRCH;
    
    // is the target proc one of our children?
    lock_acquire(targetproc->lk);
    if(targetproc->parent_id == mypid)
    {
        // it is a child!  Wait for it!
        cv_wait(targetproc->cv, targetproc->lk);
        *exitcode = targetproc->exitcode;
        lock_release(targetproc->lk);
        return 0;       // success!
    }
    else
    {
        // it is not a child!  We're uninterested
        lock_release(targetproc->lk);
        return ECHILD;
    }
}


/*
 * Create a proc structure.
 */
static
struct proc *
proc_create(const char *name)
{
	struct proc *proc;

	proc = kmalloc(sizeof(*proc));
	if (proc == NULL) {
		return NULL;
	}
	proc->p_name = kstrdup(name);
	if (proc->p_name == NULL) {
		kfree(proc);
		return NULL;
	}
    
    proc->cv = cv_create("proc_cv");
    if(proc->cv == NULL) {
        kfree(proc->p_name);
        kfree(proc);
        return NULL;
    }
    
    proc->lk = lock_create("proc_lk");
    if(proc->lk == NULL) {
        cv_destroy(proc->cv);
        kfree(proc->p_name);
        kfree(proc);
        return NULL;
    }
    
    if(ptbl_addproc(proc)) {
        lock_destroy(proc->lk);
        cv_destroy(proc->cv);
        kfree(proc->p_name);
        kfree(proc);
        return NULL;
    }
    

    spinlock_init(&proc->p_lock);
	threadarray_init(&proc->p_threads);

	/* VM fields */
	proc->p_addrspace = NULL;

	/* VFS fields */
	proc->p_cwd = NULL;

#ifdef UW
	proc->console = NULL;
#endif // UW

	return proc;
}

/*
 * Destroy a proc structure.
 */
void
proc_destroy(struct proc *proc)
{
	/*
         * note: some parts of the process structure, such as the address space,
         *  are destroyed in sys_exit, before we get here
         *
         * note: depending on where this function is called from, curproc may not
         * be defined because the calling thread may have already detached itself
         * from the process.
	 */

	KASSERT(proc != NULL);
	KASSERT(proc != kproc);

	/*
	 * We don't take p_lock in here because we must have the only
	 * reference to this structure. (Otherwise it would be
	 * incorrect to destroy it.)
	 */

	/* VFS fields */
	if (proc->p_cwd) {
		VOP_DECREF(proc->p_cwd);
		proc->p_cwd = NULL;
	}


#ifndef UW  // in the UW version, space destruction occurs in sys_exit, not here
	if (proc->p_addrspace) {
		/*
		 * In case p is the currently running process (which
		 * it might be in some circumstances, or if this code
		 * gets moved into exit as suggested above), clear
		 * p_addrspace before calling as_destroy. Otherwise if
		 * as_destroy sleeps (which is quite possible) when we
		 * come back we'll be calling as_activate on a
		 * half-destroyed address space. This tends to be
		 * messily fatal.
		 */
		struct addrspace *as;

		as_deactivate();
		as = curproc_setas(NULL);
		as_destroy(as);
	}
#endif // UW

#ifdef UW
	if (proc->console) {
	  vfs_close(proc->console);
	}
#endif // UW

	threadarray_cleanup(&proc->p_threads);
	spinlock_cleanup(&proc->p_lock);
    
    remove_from_proc_table(proc);

	kfree(proc->p_name);
    
    // do not kfree proc here because the proc can still exist in our proc_table
    //    instead, call ptbl_removeproc and that will kfree it (if it's ready to be kfreed)
    ptbl_removeproc(proc);

#ifdef UW
	/* decrement the process count */
        /* note: kproc is not included in the process count, but proc_destroy
	   is never called on kproc (see KASSERT above), so we're OK to decrement
	   the proc_count unconditionally here */
	P(proc_count_mutex); 
	KASSERT(proc_count > 0);
	proc_count--;
	/* signal the kernel menu thread if the process count has reached zero */
	if (proc_count == 0) {
	  V(no_proc_sem);
	}
	V(proc_count_mutex);
#endif // UW
	

}

/*
 * Create the process structure for the kernel.
 */
void
proc_bootstrap(void)
{
    proc_table = NULL;
    proc_table_mutex = NULL;
    
  kproc = proc_create("[kernel]");
  if (kproc == NULL) {
    panic("proc_create for kproc failed\n");
  }
  
  ptbl_create();
  
#ifdef UW
  proc_count = 0;
  proc_count_mutex = sem_create("proc_count_mutex",1);
  if (proc_count_mutex == NULL) {
    panic("could not create proc_count_mutex semaphore\n");
  }
  no_proc_sem = sem_create("no_proc_sem",0);
  if (no_proc_sem == NULL) {
    panic("could not create no_proc_sem semaphore\n");
  }
#endif // UW 
}

/*
 * Create a fresh proc for use by runprogram.
 *
 * It will have no address space and will inherit the current
 * process's (that is, the kernel menu's) current directory.
 */
struct proc *
proc_create_runprogram(const char *name)
{
	struct proc *proc;
	char *console_path;

	proc = proc_create(name);
	if (proc == NULL) {
		return NULL;
	}

#ifdef UW
	/* open the console - this should always succeed */
	console_path = kstrdup("con:");
	if (console_path == NULL) {
	  panic("unable to copy console path name during process creation\n");
	}
	if (vfs_open(console_path,O_WRONLY,0,&(proc->console))) {
	  panic("unable to open the console during process creation\n");
	}
	kfree(console_path);
#endif // UW
	  
	/* VM fields */

	proc->p_addrspace = NULL;

	/* VFS fields */

#ifdef UW
	/* we do not need to acquire the p_lock here, the running thread should
           have the only reference to this process */
        /* also, acquiring the p_lock is problematic because VOP_INCREF may block */
	if (curproc->p_cwd != NULL) {
		VOP_INCREF(curproc->p_cwd);
		proc->p_cwd = curproc->p_cwd;
	}
#else // UW
	spinlock_acquire(&curproc->p_lock);
	if (curproc->p_cwd != NULL) {
		VOP_INCREF(curproc->p_cwd);
		proc->p_cwd = curproc->p_cwd;
	}
	spinlock_release(&curproc->p_lock);
#endif // UW

#ifdef UW
	/* increment the count of processes */
        /* we are assuming that all procs, including those created by fork(),
           are created using a call to proc_create_runprogram  */
	P(proc_count_mutex); 
	proc_count++;
	V(proc_count_mutex);
#endif // UW

	return proc;
}

/*
 * Add a thread to a process. Either the thread or the process might
 * or might not be current.
 */
int
proc_addthread(struct proc *proc, struct thread *t)
{
	int result;

	KASSERT(t->t_proc == NULL);

	spinlock_acquire(&proc->p_lock);
	result = threadarray_add(&proc->p_threads, t, NULL);
	spinlock_release(&proc->p_lock);
	if (result) {
		return result;
	}
	t->t_proc = proc;
	return 0;
}

/*
 * Remove a thread from its process. Either the thread or the process
 * might or might not be current.
 */
void
proc_remthread(struct thread *t)
{
	struct proc *proc;
	unsigned i, num;

	proc = t->t_proc;
	KASSERT(proc != NULL);

	spinlock_acquire(&proc->p_lock);
	/* ugh: find the thread in the array */
	num = threadarray_num(&proc->p_threads);
	for (i=0; i<num; i++) {
		if (threadarray_get(&proc->p_threads, i) == t) {
			threadarray_remove(&proc->p_threads, i);
			spinlock_release(&proc->p_lock);
			t->t_proc = NULL;
			return;
		}
	}
	/* Did not find it. */
	spinlock_release(&proc->p_lock);
	panic("Thread (%p) has escaped from its process (%p)\n", t, proc);
}

/*
 * Fetch the address space of the current process. Caution: it isn't
 * refcounted. If you implement multithreaded processes, make sure to
 * set up a refcount scheme or some other method to make this safe.
 */
struct addrspace *
curproc_getas(void)
{
	struct addrspace *as;
#ifdef UW
        /* Until user processes are created, threads used in testing 
         * (i.e., kernel threads) have no process or address space.
         */
	if (curproc == NULL) {
		return NULL;
	}
#endif

	spinlock_acquire(&curproc->p_lock);
	as = curproc->p_addrspace;
	spinlock_release(&curproc->p_lock);
	return as;
}

/*
 * Change the address space of the current process, and return the old
 * one.
 */
struct addrspace *
curproc_setas(struct addrspace *newas)
{
	struct addrspace *oldas;
	struct proc *proc = curproc;

	spinlock_acquire(&proc->p_lock);
	oldas = proc->p_addrspace;
	proc->p_addrspace = newas;
	spinlock_release(&proc->p_lock);
	return oldas;
}
