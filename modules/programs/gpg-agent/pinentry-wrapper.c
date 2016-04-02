#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

/* Get the terminal path of the given PID and FD using the /proc file system. */
char *get_terminal(pid_t pid, int fd)
{
    int term, is_term;
    ssize_t linklen;
    char fd_path[50];
    char term_path[100];
    struct stat st;

    if (snprintf(fd_path, 50, "/proc/%d/fd/%d", pid, fd) < 0) {
        perror("snprintf proc fd path");
        return NULL;
    }

    if (lstat(fd_path, &st) == -1)
        return NULL;

    if (!S_ISLNK(st.st_mode))
        return NULL;

    if ((linklen = readlink(fd_path, term_path, sizeof term_path)) == -1) {
        perror("readlink term path");
        return NULL;
    }

    term_path[linklen] = 0;

    if ((term = open(term_path, O_RDONLY | O_NOCTTY)) == -1)
        return NULL;

    is_term = isatty(term);

    if (close(term) == -1) {
        perror("close client tty");
        return NULL;
    }

    if (!is_term)
        return NULL;

    return strdup(term_path);
}

/* Probes FD 0, 1 and 2 for a connected terminal device and return an allocated
 * string pointing to the filename.
 */
char *detect_terminal(pid_t pid)
{
    char *term;

    for (int i = 0; i < 3; ++i) {
        term = get_terminal(pid, i);
        if (term == NULL)
            continue;

        return term;
    }

    return NULL;
}

/* Fetch the info from /proc/PID/environ and retorn it as an array. */
char **fetch_environ(pid_t pid)
{
    char environ_path[50], **result = NULL;
    char buf[2048], *envbuf, *environ = NULL;
    size_t chunklen, envlen = 0;
    int env_fd;

    if (snprintf(environ_path, 50, "/proc/%d/environ", pid) < 0) {
        perror("snprintf proc environ path");
        return NULL;
    }

    if ((env_fd = open(environ_path, O_RDONLY)) == -1) {
        perror("open proc environ");
        return NULL;
    }

    while ((chunklen = read(env_fd, buf, sizeof buf)) > 0) {
        if (environ == NULL) {
            if ((environ = malloc(envlen + chunklen + 1)) == NULL) {
                perror("malloc proc environ");
                return NULL;
            }
        } else {
            if ((environ = realloc(environ, envlen + chunklen + 1)) == NULL) {
                perror("realloc proc environ");
                free(environ);
                return NULL;
            }
        }
        memcpy(environ + envlen, buf, chunklen);
        envlen += chunklen;
        environ[envlen + 1] = 0;
        if (chunklen < sizeof buf)
            break;
    }

    if (close(env_fd) == -1) {
        perror("close proc environ");
        free(environ);
        return NULL;
    }

    envbuf = environ;

    if ((result = malloc(sizeof(char*))) == NULL) {
        perror("malloc environ array");
        free(environ);
        return NULL;
    }
    result[0] = NULL;

    for (int i = 0; envbuf - environ < envlen; ++i) {
        if ((result = realloc(result, sizeof(char*) * (i + 2))) == NULL) {
            perror("realloc environ array");
            free(environ);
            free(result);
            return NULL;
        }

        result[i] = strndup(envbuf, envlen - (envbuf - environ));
        result[i + 1] = NULL;
        envbuf += strlen(envbuf) + 1;
    }

    free(environ);
    return result;
}

void free_environ(char **environ)
{
    char **tmp = environ;
    if (environ == NULL) return;
    do free(*tmp);
    while (*(++tmp) != NULL);
    free(environ);
    environ = NULL;
}

struct proc_info {
    char **environ;
    char *term;
};

/* Gather information for the given process ID, like environment or connected
 * terminals.
 */
struct proc_info *open_proc_info(pid_t pid)
{
    struct proc_info *pi = NULL;

    if ((pi = malloc(sizeof(struct proc_info *))) == NULL) {
        perror("malloc proc_info");
        return NULL;
    }

    pi->term = detect_terminal(pid);
    if ((pi->environ = fetch_environ(pid)) == NULL) {
        free(pi->term);
        free(pi);
        return NULL;
    }

    return pi;
}

void close_proc_info(struct proc_info *pi)
{
    if (pi->term != NULL) free(pi->term);
    free_environ(pi->environ);
    free(pi);
}

/* Fetch an environment variable from the proc_info structure similar to
 * getenv() but for remote PIDs.
 */
char *proc_info_getenv(struct proc_info *pi, const char *name)
{
    char **tmp = pi->environ;
    size_t namelen = strlen(name);
    do {
        if (strncmp(*tmp, name, namelen) == 0 &&
            *(*tmp + namelen) == '=') {
            return strdup(*tmp + namelen + 1);
        }
    } while (*(++tmp) != NULL);
    return NULL;
}

#define MAYBE_EXPAND_ARGV(opt, value) \
    if ((tmp = value) != NULL) { \
        new_argv = realloc(new_argv, sizeof(char*) * (new_argc + 3)); \
        if (new_argv == NULL) { \
            perror("realloc new argv"); \
            return EXIT_FAILURE; \
        } \
        new_argv[new_argc + 0] = "--" opt; \
        new_argv[new_argc + 1] = tmp; \
        new_argv[new_argc + 2] = NULL; \
        new_argc += 2; \
    }

/* This is our program main routine whenever we get a _CLIENT_PID environment
 * variable.
 */
int wrap(struct proc_info *pi, int argc, char **argv)
{
    char *tmp, **new_argv;
    int new_argc = 1;

    if ((new_argv = malloc(sizeof(char*) * 2)) == NULL) {
        perror("malloc new argv");
        return EXIT_FAILURE;
    }

    new_argv[0] = PINENTRY_PROGRAM;
    new_argv[1] = NULL;

    MAYBE_EXPAND_ARGV("display", proc_info_getenv(pi, "DISPLAY"));
    MAYBE_EXPAND_ARGV("ttyname", strdup(pi->term));
    MAYBE_EXPAND_ARGV("ttytype", proc_info_getenv(pi, "TERM"));
    MAYBE_EXPAND_ARGV("lc-ctype", proc_info_getenv(pi, "LC_CTYPE"));
    MAYBE_EXPAND_ARGV("lc-messages", proc_info_getenv(pi, "LC_MESSAGES"));

    close_proc_info(pi);

    /* No DISPLAY/TTY found, so use the arguments provided by the agent. */
    if (new_argc == 1) {
        free(new_argv);
        new_argv = argv;
    }

    /* Make sure we don't have DISPLAY already in our environment to avoid
     * starting a pinentry on X while the user is connected via SSH for example.
     */
    if (unsetenv("DISPLAY") == -1)
        return EXIT_FAILURE;

    if (execv(PINENTRY_PROGRAM, new_argv) == -1) {
        perror("execv real pinentry");
        return EXIT_FAILURE;
    }

    /* Not reached because the process should be substituted in execve(). */
    return EXIT_SUCCESS;
}

int main(int argc, char **argv)
{
    const char *pidstr;
    struct proc_info *pi = NULL;

    if ((pidstr = getenv("_CLIENT_PID")) != NULL) {
        if ((pi = open_proc_info(atoi(pidstr))) == NULL)
            fprintf(stderr, "Client PID %d has vanished before we could"
                    " retrieve /proc information.\n", atoi(pidstr));
        else
            return wrap(pi, argc, argv);
    }

    argv[0] = PINENTRY_PROGRAM;

    if (execv(PINENTRY_PROGRAM, argv) == -1) {
        perror("execv real pinentry");
        return EXIT_FAILURE;
    }

    /* Not reached because the process should be substituted in execve(). */
    return EXIT_SUCCESS;
}
