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

This probably could go in a separate source file but I don't want to have to deal with
modifying the makefiles.  So fuggit.

This is probably not the most efficient, as most operations on proc creation/destruction are
O(n) since I walk through the proc list.


We want parent processes to always be able to check a child pid, even if that child has exited.
SO -- when a process closes, the pid stays reserved until the PARENT closes.

The exception is for children of kproc, which are released immediately

***********************************************/
struct ptbl_entry
{
    struct proc*    p;              // pointer to the proc struct (or NULL if the proc has exited)
    int             exit_code;      // the exit code of the proc
    pid_t           parent_pid;     // pid of the parent process, (or zero if the parent process has exited already)
    struct cv*      wait_cv;        // CV for waitpid  (or NULL if this ptbl_entry does not exist anymore)
    struct lock*    wait_lock;      // lock for wait_cv  (or NULL if "  "   "  ")
};

#define MAX_PTBL_SIZE       (__PID_MAX - PIX_MIN + 1)
#define START_PTBL_SIZE     20

static struct semaphore* proc_table_mutex;
static struct ptbl_entry* proc_table;           // I probably could use the 'array' lib for this, but I looked at it and it's a pain
static unsigned proc_table_size;                //      and really, all I need is a simple realloc

/**********************
*  Functions for the proc table!!!!
***********************/
static void clear_ptbl_entry(struct ptbl_entry* e)
{
    e->p            = NULL;
    e->exit_code    = 0;
    e->parent_pid   = 0;
    e->wait_cv      = NULL;
    e->wait_lock    = NULL;
}

static void destroy_ptbl_entry(struct ptbl_entry* e)
{
    if(e->wait_cv)      cv_destroy(e->wait_cv);
    if(e->wait_lock)    lock_destroy(e->wait_lock);
    clear_ptbl_entry(e);
}

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
    for(i = 0; i < proc_table_size; ++i)    clear_ptbl_entry( &proc_table[i] );
    
    proc_table_mutex = sem_create("proc_table_mutex",1);
    if (proc_table_mutex) {
        panic("could not create proc_table_mutex semaphore\n");
    }
}

static int
unsafe_ptbl_realloc()
{
    unsigned i;
    unsigned newsize;
    struct ptbl_entry* x;
    
    newsize = proc_table_size * 2;
    if(newsize > MAX_PTBL_SIZE)     newsize = MAX_PTBL_SIZE;
    if(newsize <= proc_table_size)  return 1;       // can't allocate any more
    
    x = kmalloc( sizeof(*x) * newsize );
    if(x == NULL)                   return 1;       // no more mem!!
    
    for(i = 0; i < proc_table_size; ++i)        x[i] = proc_table[i];
    for(i = proc_table_size; i < newsize; ++i)  clear_ptbl_entry(&x[i]);
    
    kfree(proc_table);
    proc_table = x;
    proc_table_size = newsize;
    return 0;
}

static int
ptbl_addproc(struct proc* proc, pid_t parent)
{
    unsigned i;
    
    P(proc_table_mutex);
    // find an empty slot
    for(i = 0; i < proc_table_size; ++i)
    {
        if(proc_table[i].wait_cv == NULL)       break;  // slot i is available!
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
    
    // Create the CV and Lock
    proc_table[i].wait_cv = cv_create("ptbl_cv");
    if(proc_table[i].wait_cv == NULL) {
        V(proc_table_mutex);
        return 1;
    }
    proc_table[i].wait_lock = lock_create("ptbl_lock");
    if(proc_table[i].wait_lock == NULL) {
        cv_destroy(proc_table[i].wait_cv);
        proc_table[i].wait_cv = NULL;
        V(proc_table_mutex);
        return 1;
    }
    
    // success!!  fill in the rest of the entry!
    proc_table[i].exit_code = 0;
    proc_table[i].p = proc;
    proc_table[i].parent_pid = parent;
    proc->pid = (pid_t)(i + __PID_MIN);
    
    V(proc_table_mutex);
    return 0;
}

static void
ptbl_removeproc(struct proc* p)
{
    unsigned i;
    struct cv* cv;          // this is kind of gross....
    struct lock* lk;
    
    KASSERT(p != NULL);
    KASSERT(p->pid >= __PID_MIN);
    KASSERT(p->pid <= __PID_MAX);
    
    P(proc_table_mutex);
    
    // Look for our children and clean them up / mark them to be cleaned up
    for(i = 0; i < proc_table_size; ++i)        
    {
        if(proc_table[i].wait_cv == NULL)       continue;   // this entry is already empty -- skip it
        
        if(proc_table[i].parent_pid == p->pid)      // this entry is one of our children
        {
            if(proc_table[i].p == NULL) {           // it's already exited, clean it up now
                destroy_ptbl_entry(&proc_table[i]);
            } else {                                // it's still alive, erase it's parent pid so that it will auto destroy when it exits
                proc_table[i].parent_pid = 0;
            }
        }
    }
    
    // now... remove US!
    i = p->pid - __PID_MIN;
    KASSERT(proc_table[i].p == p);
    
    // if our parent has been destroyed already, or if our parent is kproc, destroy ourselves!
    if(proc_table[i].parent_pid == 0 || proc_table[i].parent_pid == 1)
    {
        destroy_ptbl_entry(&proc_table[i]);
        V(proc_table_mutex);
    }
    else
    {
        // otherwise, leave the entry in tact, but signal that we've closed
        proc_table[i].p = NULL;
        cv = proc_table[i].wait_cv;
        lk = proc_table[i].wait_lk;
        
        V(proc_table_mutex);
        lock_acquire( lk );
        cv_broadcast( cv, lk );
        lock_release( lk );
    }
}

/* Set the exit code for a process */
void proc_setexitcode(struct proc* proc, int exitcode);
{
    unsigned i;
    
    KASSERT(proc != NULL);
    KASSERT(proc->pid >= __PID_MIN);
    KASSERT(proc->pid <= __PID_MAX);
    
    P(proc_table_mutex);
    
    i = proc->pid - __PID_MIN;
    KASSERT( i < proc_table_size );
    KASSERT( proc_table[i].p == proc );
    
    proc_table[i].exit_code = exitcode;
    V(proc_table_mutex);
}

pid_t proc_waitpid(pid_t pid, int* exitcode)
{
    struct cv* cv;          // this is kind of gross....
    struct lock* lk;
    unsigned i;
    
    KASSERT(pid >= __PID_MIN);
    KASSERT(pid <= __PID_MAX);
    
    i = pid - __PID_MIN;
    
    P(proc_table_mutex);
    KASSERT(i < proc_table_size);
    
    cv = proc_table[i].wait_cv;
    lk = proc_table[i].wait_lk;
    
    // we can only check this pid if we are its parent, otherwise return "pid isn't valid"
    if(proc_table[i].parent_pid != curproc->pid || cv == NULL)
    {
        V(proc_table_mutex);
        *exitcode = 0;
        return -1;
    }
    else
    {
        // did the proc exit already?
        if(proc_table[i].p == NULL)
        {
            V(proc_table_mutex);
            *exitcode = proc_table[i].exit_code;
        }
        else
        {
            V(proc_table_mutex);
            lock_acquire(lk);
            cv_wait(cv,lk);
            //// TODO I'm unhappy with all this crap  REDO
        }
    }
    
    return pid;
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
    
    if(add_proc_to_table(proc)) {
        kfree(proc->p_name);
        kfree(proc);
        return NULL;
    }

	threadarray_init(&proc->p_threads);
	spinlock_init(&proc->p_lock);

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
	kfree(proc);

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
    int i;
    
    proc_table = NULL;
  kproc = proc_create("[kernel]");
  if (kproc == NULL) {
    panic("proc_create for kproc failed\n");
  }
  
  create_ptbl();
  
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
