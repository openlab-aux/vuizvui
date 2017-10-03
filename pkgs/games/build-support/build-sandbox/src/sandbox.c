#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "setup.h"

int main(int argc, char **argv)
{
    if (!setup_sandbox())
        return 1;

    argv[0] = WRAPPED_PROGNAME;
    if (execv(WRAPPED_PATH, argv) == -1) {
        fprintf(stderr, "exec %s: %s\n", WRAPPED_PATH, strerror(errno));
        return 1;
    }

    // Should never be reached.
    return 1;
}
