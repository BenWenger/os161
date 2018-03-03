/*
 * An example program.
 */
#include <unistd.h>

int
main()
{
    pid_t result, mypid;
    
    result = fork();
    
    if(result)          // we are the parent
    {
        waitpid(result, NULL, 0);
        printf("PARENT THREAD:  My child was %d, I am %d\n", result, get_pid());
    }
    else
    {
        printf("CHILD THREAD:  My pid is %d", get_pid());
    }
	return 0; /* avoid compiler warnings */
}
