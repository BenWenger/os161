#include <types.h>
#include <kern/errno.h>
#include <kern/unistd.h>
#include <kern/wait.h>
#include <lib.h>
#include <syscall.h>
#include <current.h>
#include <proc.h>
#include <thread.h>
#include <addrspace.h>
#include <copyinout.h>
#include <mips/trapframe.h>

  /* this implementation of sys__exit does not do anything with the exit code */
  /* this needs to be fixed to get exit() and waitpid() working properly */

void sys__exit(int exitcode) {

  struct addrspace *as;
  struct proc *p = curproc;
  /* for now, just include this to keep the compiler from complaining about
     an unused variable */
  (void)exitcode;

  DEBUG(DB_SYSCALL,"Syscall: _exit(%d)\n",exitcode);

  KASSERT(curproc->p_addrspace != NULL);
  as_deactivate();
  /*
   * clear p_addrspace before calling as_destroy. Otherwise if
   * as_destroy sleeps (which is quite possible) when we
   * come back we'll be calling as_activate on a
   * half-destroyed address space. This tends to be
   * messily fatal.
   */
  as = curproc_setas(NULL);
  as_destroy(as);

  /* detach this thread from its process */
  /* note: curproc cannot be used after this call */
  proc_remthread(curthread);

  /* if this is the last user process in the system, proc_destroy()
     will wake up the kernel menu thread */
  proc_destroy(p);
  
  thread_exit();
  /* thread_exit() does not return, so we should never get here */
  panic("return from thread_exit in sys_exit\n");
}

static void
enter_fork_thread(void* temp_tf, unsigned long unused)
{
    // Apparently the trap frame needs to exist on the stack of this thread
    //   so copy that temporary tf onto our stack
    struct trapframe tf = *((struct trapframe*)(temp_tf));
    kfree(temp_tf);
    tf.tf_v0 = 0;       // return value of zero to indicate this is the child process
    tf.tf_epc += 4;     // move past the syscall
    mips_usermode(&tf);
    
    (void)unused;
}

int
sys_fork(pid_t *retval, struct trapframe *tf)
{
    struct proc* childproc;
    struct trapframe* childtf;
    int result;
    
    // Create a new user process
    //   proc_create_runprogram says that it expects to be called for all new procs -- including those
    //   made with fork(). So use it here to make the child process.
    childproc = proc_create_runprogram("forked");       // that's a bad name, whatever
    if(childproc == NULL) {
        return EMPROC;  // probably wrong error code but I don't see one that fits better
    }
    
    // We shouldn't have an AS yet -- copy it from this proc
    KASSERT(childproc->p_addrspace == NULL);
    result = as_copy( curproc->p_addrspace, &childproc->p_addrspace );
    if(result) {
        proc_destroy(childproc);
        return result;
    }
    
    // Rather than make a semaphore and lock on it to copy the trapframe, just make a copy of it now
    childtf = kmalloc(sizeof(struct trapframe));
    if(!childtf) {
        proc_destroy(childproc);
        return ENOMEM;
    }
    *childtf = *tf;
    
    // So now we have duplicated all the memory with as_copy.  We have duplicated all the regs with 
    //   the trapframe.  The trapframe also indicates where the PC is, so we just need to enter user
    //   code at that new trapframe and our process will be forked!
    result = thread_fork("forked_t", childproc,
        &enter_fork_thread, childtf, 0);
    if(result)
    {
        kfree(childtf);
        proc_destroy(childproc);
        return result;
    }
    
    *retval = 2;        // todo replace this with the actual PID
    return 0;
}


/* stub handler for getpid() system call                */
int
sys_getpid(pid_t *retval)
{
  /* for now, this is just a stub that always returns a PID of 1 */
  /* you need to fix this to make it work properly */
  *retval = curproc->pid;
  return(0);
}

/* stub handler for waitpid() system call                */

int
sys_waitpid(pid_t pid,
	    userptr_t status,
	    int options,
	    pid_t *retval)
{
  int exitstatus;
  int result;

  /* this is just a stub implementation that always reports an
     exit status of 0, regardless of the actual exit status of
     the specified process.   
     In fact, this will return 0 even if the specified process
     is still running, and even if it never existed in the first place.

     Fix this!
  */

  if (options != 0) {
    return(EINVAL);
  }
  /* for now, just pretend the exitstatus is 0 */
  exitstatus = 0;
  result = copyout((void *)&exitstatus,status,sizeof(int));
  if (result) {
    return(result);
  }
  *retval = pid;
  return(0);
}

