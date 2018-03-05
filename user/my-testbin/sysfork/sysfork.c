/*
 * An example program.
 */
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>

int
main()
{
    int i;
    pid_t result;
    
    result = fork();
    
    if(result)          // we are the parent
    {
        waitpid(result, &i, 0);
        printf("PARENT THREAD:  My child was %d, I am %d\n", result, getpid());
        printf("Parent:  my child exited with code %d\n", i);
    }
    else
    {
        printf("CHILD THREAD:  My pid is %d\n", getpid());
        printf("Child:  I'm going to waste a lot of time so that the parent has to wait.\n");
        for(i = 0; i < 20000; ++i)
        {
            if((i % 1000) == 0)
                printf(".\n");
        }
        printf("Child tread is done.  Exiting with code 5!\n\n");
        return 5;
    }
	return 0;
}
