
/*
 * getpid.c - getpid system call test
 */
#include <unistd.h>
#include <stdio.h>

int
main()
{
        pid_t pid;

        printf("Test call for getpid stub\n");
        pid = getpid();
        printf("pid: %d\n", pid);
        return 0; /* avoid compiler warnings */
}
