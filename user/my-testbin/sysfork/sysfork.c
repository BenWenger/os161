/*
 * An example program.
 */
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>

int
main()
{
    pid_t result;
    
    result = fork();
    
    if(result)          // we are the parent
    {
        waitpid(result, NULL, 0);
        printf("PARENT THREAD:  My child was %d, I am %d\n", result, getpid());
    }
    else
    {
        printf("CHILD THREAD:  My pid is %d\n", getpid());
    }
	return 0;
}


/*
int
main()
{
    pid_t result;
    
    result = fork();
    
    if(result)          // we are the parent
    {
        for(int i = 0; i < 20; ++i) {
            printf("P");
        }
    }
    else
    {
        for(int i = 0; i < 20; ++i) {
            printf("C");
        }
    }
	return 0;
}
*/
