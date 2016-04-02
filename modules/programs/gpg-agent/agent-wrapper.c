#define _GNU_SOURCE
#include <dlfcn.h>

#include <stddef.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <malloc.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <systemd/sd-daemon.h>

int main_fd = 0;
int ssh_fd = 0;
int scdaemon_fd = 0;

/* Get the systemd file descriptor for a particular socket file.
 * Returns -1 if there is an error or -2 if it is an unnamed socket.
 */
int get_sd_fd_for(const struct sockaddr_un *addr)
{
    if (main_fd == 0 && ssh_fd == 0 && scdaemon_fd == 0) {
        int num_fds;
        char **fdmap = NULL;
        num_fds = sd_listen_fds_with_names(0, &fdmap);

        if (num_fds <= 0) {
            fputs("No suitable file descriptors in LISTEN_FDS.\n", stderr);
            if (num_fds == 0)
                errno = EADDRNOTAVAIL;
            return -1;
        }

        if (fdmap != NULL) {
            for (int i = 0; i < num_fds; i++) {
                if (strncmp(fdmap[i], "main", 5) == 0)
                    main_fd = SD_LISTEN_FDS_START + i;
                else if (strncmp(fdmap[i], "ssh", 4) == 0)
                    ssh_fd = SD_LISTEN_FDS_START + i;
                else if (strncmp(fdmap[i], "scdaemon", 9) == 0)
                    scdaemon_fd = SD_LISTEN_FDS_START + i;
                free(fdmap[i]);
            }
            free(fdmap);
        }
    }

    if (addr->sun_path == NULL || *(addr->sun_path) == 0)
        return -2;

    char *basename = strrchr(addr->sun_path, '/');
    if (basename == NULL) {
        fprintf(stderr, "Socket path %s is not absolute.\n",
                addr->sun_path);
        errno = EADDRNOTAVAIL;
        return -1;
    } else {
        basename++;
    }

    if (strncmp(basename, "S.gpg-agent", 12) == 0) {
        return main_fd;
    } else if (strncmp(basename, "S.gpg-agent.ssh", 16) == 0) {
        return ssh_fd;
    } else if (strncmp(basename, "S.scdaemon", 11) == 0) {
        return scdaemon_fd;
    } else {
        fprintf(stderr, "Socket path %s is unknown.\n", addr->sun_path);
        errno = EADDRNOTAVAIL;
        return -1;
    }
}

/* Replace the systemd-provided socket FD with the one that is used by the
 * agent, so that we can later look it up in our accept() wrapper.
 */
void record_sockfd(int sysd_fd, int redir_fd)
{
    if (sysd_fd == main_fd)
        main_fd = redir_fd;
    else if (sysd_fd == ssh_fd)
        ssh_fd = redir_fd;
}

/* systemd is already listening on that socket, so we don't need to. */
int listen(int sockfd, int backlog)
{
    return 0;
}

/* The agent should already have called socket() before and we need to close the
 * file descriptor that the socket() call has returned and replace it with the
 * one provided by systemd.
 */
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
    int new_fd;

    new_fd = get_sd_fd_for((const struct sockaddr_un *)addr);

    switch (new_fd) {
        case -1: return -1;
        /* Unnamed socket, just pretend everything is fine */
        case -2: return 0;
    }

    if ((new_fd = get_sd_fd_for((const struct sockaddr_un *)addr)) == -1)
        return -1;

    fprintf(stderr, "bind: Redirecting FD %d to systemd-provided FD %d.\n",
            sockfd, new_fd);

    if (dup2(new_fd, sockfd) == -1)
        return -1;
    else
        record_sockfd(new_fd, sockfd);

    return 0;
}

/* Avoid forking for the first time so we can properly track the agent using a
 * systemd service (without the need to set Type="forking").
 */
int first_fork = 1;

pid_t fork(void)
{
    static pid_t (*_fork)(void) = NULL;
    if (_fork == NULL)
        _fork = dlsym(RTLD_NEXT, "fork");

    /* Unset the LD_PRELOAD environment variable to make sure we don't propagate
     * it down to things like the pinentry.
     */
    if (unsetenv("LD_PRELOAD") == -1)
        return -1;

    if (first_fork)
        return first_fork = 0;

    return _fork();
}

/* Get the PID of the client connected to the given socket FD. */
pid_t get_socket_pid(int sockfd)
{
    struct ucred pcred;
    socklen_t pcred_len = sizeof(pcred);

    if (getsockopt(sockfd, SOL_SOCKET, SO_PEERCRED, &pcred, &pcred_len) == -1)
        return -1;

    return pcred.pid;
}

pid_t last_pid = 0;

/* For the pinentry to work correctly with SSH, we need to record the process ID
 * of the process communicating with the agent. That way we can get more
 * information about the PTS/PTY/TTY the user is on and also know whether a
 * DISPLAY is set for that process, because we will connect the pinentry's TTY
 * to the TTY of the process on the other end of the socket.
 */
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
    int retval;

    static int (*_accept)(int, struct sockaddr *, socklen_t *) = NULL;
    if (_accept == NULL)
        _accept = dlsym(RTLD_NEXT, "accept");

    retval = _accept(sockfd, addr, addrlen);

    last_pid = 0;

    if (retval != -1 && ssh_fd != 0 && sockfd == ssh_fd) {
        pid_t client_pid = get_socket_pid(retval);
        if (client_pid == -1) {
            close(retval);
            return -1;
        }
        last_pid = client_pid;
        fprintf(stderr, "Socket endpoint PID for accepted socket %d is %d.\n",
                retval, client_pid);
    }

    return retval;
}

/* Wrap the execv() that calls the pinentry program to include a special
 * _CLIENT_PID environment variable, which contains the PID we gathered during
 * accept(). Note that this is potentially racy if we have a lot of concurrent
 * connections, but the worst that could happen is that we end up having a
 * pinentry running on the wrong TTY/display.
 */
int execv(const char *path, char *const argv[])
{
    static int (*_execv)(const char *, char *const[]) = NULL;
    if (_execv == NULL)
        _execv = dlsym(RTLD_NEXT, "execv");

    if (last_pid != 0 &&
        strncmp(path, PINENTRY_WRAPPER, sizeof(PINENTRY_WRAPPER) + 1) == 0) {
        char env_var[40];
        if (snprintf(env_var, 40, "_CLIENT_PID=%d", last_pid) < 0)
            return -1;
        if (putenv(env_var) < 0)
            return -1;
    }

    last_pid = 0;
    return _execv(path, argv);
}
