#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L

#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <malloc.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static bool write_proc(int proc_pid_fd, const char *fname, const char *buf,
                       size_t buflen, bool ignore_errors)
{
    int fd;

    if ((fd = openat(proc_pid_fd, fname, O_WRONLY)) == -1) {
        fprintf(stderr, "open %s: %s\n", fname, strerror(errno));
        return false;
    }

    if (write(fd, buf, buflen) == -1) {
        if (!ignore_errors)
            fprintf(stderr, "write %s: %s\n", fname, strerror(errno));
        close(fd);
        return ignore_errors;
    }

    close(fd);
    return true;
}

#define WRITE_IDMAP(file, value) \
    buflen = snprintf(buf, 100, "%1$lu %1$lu 1", (unsigned long)value); \
    if (buflen >= 100) { \
        fputs("Unable to write buffer for " file ".\n", stderr); \
        close(proc_pid_fd); \
        return false; \
    } else if (buflen < 0) { \
        perror("snprintf " file " buffer"); \
        close(proc_pid_fd); \
        return false; \
    } \
    if (!write_proc(proc_pid_fd, file, buf, buflen, false)) { \
        close(proc_pid_fd); \
        return false; \
    }

static bool write_maps(pid_t parent_pid)
{
    int proc_pid_fd;
    size_t buflen;
    char buf[100];

    buflen = snprintf(buf, 100, "/proc/%lu", (unsigned long)parent_pid);
    if (buflen >= 100) {
        fputs("Unable to write buffer for child pid proc path.\n", stderr);
        return false;
    } else if (buflen < 0) {
        perror("snprintf child pid proc path");
        return false;
    }

    if ((proc_pid_fd = open(buf, O_RDONLY | O_DIRECTORY)) == -1) {
        fprintf(stderr, "open %s: %s\n", buf, strerror(errno));
        return false;
    }

    WRITE_IDMAP("uid_map", geteuid());

    // Kernels prior to Linux 3.19 which do not impose setgroups()
    // restrictions won't have this file, so ignore failure.
    write_proc(proc_pid_fd, "setgroups", "deny", 4, true);

    WRITE_IDMAP("gid_map", getegid());

    return true;
}

static bool makedirs(const char *path)
{
    char *tmp, *segment;

    if ((tmp = strdup(path)) == NULL) {
        fprintf(stderr, "strdup of %s: %s\n", path, strerror(errno));
        return false;
    }

    segment = dirname(tmp);

    if (!(segment[0] == '/' && segment[1] == '\0')) {
        if (!makedirs(segment)) {
            free(tmp);
            return false;
        }
    }

    (void)mkdir(path, 0755);
    free(tmp);
    return true;
}

static bool bind_mount(const char *path, bool restricted)
{
    int mflags = MS_BIND | MS_REC;
    size_t srclen;
    char src[PATH_MAX], target[PATH_MAX];

    if (restricted)
        mflags |= MS_NOSUID | MS_NODEV | MS_NOATIME;

    if (realpath(path, src) == NULL) {
        fprintf(stderr, "realpath of %s: %s\n", path, strerror(errno));
        return false;
    }

    if ((srclen = strlen(src)) > PATH_MAX - 4) {
        fprintf(stderr, "`/tmp/%s' does not fit in PATH_MAX.\n", src);
        return false;
    }

    memcpy(target, "/tmp", 4);
    memcpy(target + 4, src, srclen + 1);

    if (!makedirs(target))
        return false;

    if (mount(src, target, "", mflags, NULL) == -1) {
        fprintf(stderr, "mount %s to %s: %s\n", src, target, strerror(errno));
        return false;
    }

    return true;
}

struct envar_offset {
    int start;
    int length;
    int var_start;
    int var_length;
    struct envar_offset *next;
};

static struct envar_offset *alloc_offset(void)
{
    struct envar_offset *new_offset;
    new_offset = malloc(sizeof(struct envar_offset));

    if (new_offset == NULL) {
        perror("malloc envar_offset");
        return NULL;
    }

    new_offset->next = NULL;
    return new_offset;
}

static struct envar_offset *push_offset(struct envar_offset *current,
                                        struct envar_offset **base)
{
    if (current == NULL) {
        if ((current = alloc_offset()) != NULL)
            *base = current;
        return current;
    }

    return current->next = alloc_offset();
}

static void free_offsets(struct envar_offset *base)
{
    struct envar_offset *next;
    if (base == NULL)
        return;
    next = base->next;
    free(base);
    if (next != NULL)
        free_offsets(next);
}

static char *expand_xdg_fallback(const char *xdg_var)
{
    static char *home = NULL;
    static size_t homelen;
    char *result;

    if (home == NULL) {
        if ((home = getenv("HOME")) == NULL) {
            fputs("Unable find $HOME.\n", stderr);
            return NULL;
        }
        homelen = strlen(home);
    }

    if (strcmp(xdg_var, "XDG_DATA_HOME") == 0) {
        result = malloc(homelen + 14);
        if (result == NULL) {
            perror("malloc XDG_DATA_HOME");
            return NULL;
        }
        memcpy(result, home, homelen);
        memcpy(result + homelen, "/.local/share", 14);
        return result;
    } else if (strcmp(xdg_var, "XDG_CONFIG_HOME") == 0) {
        result = malloc(homelen + 9);
        if (result == NULL) {
            perror("malloc XDG_CONFIG_HOME");
            return NULL;
        }
        memcpy(result, home, homelen);
        memcpy(result + homelen, "/.config", 9);
        return result;
    }

    return NULL;
}

static char *get_offset_var(struct envar_offset *offset, const char *haystack)
{
    char *tmp, *result;

    tmp = strndup(haystack + offset->var_start, offset->var_length);

    if (tmp == NULL) {
        perror("strndup");
        return NULL;
    }

    result = getenv(tmp);
    if (result == NULL) {
        if ((result = expand_xdg_fallback(tmp)) == NULL) {
            fprintf(stderr, "Unable find variable %s in %s\n", tmp, haystack);
            free(tmp);
            return NULL;
        }
        free(tmp);
        return result;
    }
    free(tmp);
    return strdup(result);
}

static char *replace_env_offset_free(const char *path,
                                     struct envar_offset *offset)
{
    struct envar_offset *tmp_offset;
    size_t buflen, pathlen, varlen, tmplen;
    int inpos = 0, outpos = 0;
    char *buf, *curvar;

    buflen = pathlen = strlen(path);

    if ((buf = malloc(buflen + 1)) == NULL) {
        perror("malloc replace_env buffer");
        return NULL;
    }

    while (offset != NULL) {
        if ((curvar = get_offset_var(offset, path)) == NULL) {
            free(buf);
            free_offsets(offset);
            return NULL;
        }

        varlen = strlen(curvar);
        tmplen = varlen + (buflen - offset->length);

        if (tmplen > buflen) {
            if ((buf = realloc(buf, (buflen = tmplen) + 1)) == NULL) {
                perror("realloc replace_env buffer");
                free(buf);
                free(curvar);
                free_offsets(offset);
                return NULL;
            }
        }

        memcpy(buf + outpos, path + inpos, offset->start - inpos);
        outpos += offset->start - inpos;
        inpos = offset->start;

        memcpy(buf + outpos, curvar, varlen);
        outpos += varlen;
        inpos += offset->length;

        free(curvar);

        tmp_offset = offset;
        offset = offset->next;
        free(tmp_offset);
    }

    memcpy(buf + outpos, path + inpos, pathlen - inpos);
    *(buf + outpos + (pathlen - inpos)) = '\0';

    return buf;
}

static char *replace_env(const char *path)
{
    int i = 0, start = 0, var_start = 0;
    size_t pathlen;
    bool in_var = false, curly = false;
    struct envar_offset *base = NULL, *offset = NULL;

    pathlen = strlen(path);

    while (i < pathlen) {
        if (path[i] == '$' && !curly && !in_var) {
            if (i + 1 >= pathlen)
                break;

            start = i;

            if (path[i + 1] == '{') {
                curly = true;
                var_start = i + 2;
                ++i;
            } else {
                in_var = true;
                var_start = i + 1;
            }
        } else if (in_var) {
            if (!(path[i] >= 'a' && path[i] <= 'z') &&
                !(path[i] >= 'A' && path[i] <= 'Z') &&
                !(path[i] >= '0' && path[i] <= '9') &&
                path[i] != '_'
            ) {
                in_var = false;

                if ((offset = push_offset(offset, &base)) == NULL) {
                    free_offsets(base);
                    return NULL;
                }

                offset->start = start;
                offset->length = i - start;
                offset->var_start = var_start;
                offset->var_length = i - var_start;
                continue;
            }
        } else if (curly) {
            if (path[i] == '}') {
                curly = false;

                if ((offset = push_offset(offset, &base)) == NULL) {
                    free_offsets(base);
                    return NULL;
                }

                offset->start = start;
                offset->length = (i + 1) - offset->start;
                offset->var_start = var_start;
                offset->var_length = i - offset->var_start;
            }
        }

        ++i;
    }

    if (in_var) {
        if ((offset = push_offset(offset, &base)) == NULL) {
            free_offsets(base);
            return NULL;
        }

        offset->start = start;
        offset->length = i - start;
        offset->var_start = var_start;
        offset->var_length = i - var_start;
    }

    return replace_env_offset_free(path, base);
}

static bool extra_mount(const char *path)
{
    char *expanded;
    if ((expanded = replace_env(path)) == NULL)
        return false;

    if (!bind_mount(expanded, true)) {
        free(expanded);
        return false;
    }

    free(expanded);
    return true;
}

#include PARAMS_FILE

static bool setup_chroot(void)
{
    int mflags;

    mflags = MS_NOEXEC | MS_NOSUID | MS_NODEV | MS_NOATIME;

    if (mount("none", "/tmp", "tmpfs", mflags, NULL) == -1) {
        perror("mount rootfs");
        return false;
    }

    if (!bind_mount("/dev", false))
        return false;

    if (!bind_mount("/proc", false))
        return false;

    if (!bind_mount("/sys", false))
        return false;

    if (mkdir("/tmp/tmp", 0700) == -1) {
        perror("mkdir private tmp");
        return false;
    }

    if (!setup_app_paths())
        return false;

    if (chroot("/tmp") == -1) {
        perror("chroot");
        return false;
    }

    if (chdir("/") == -1) {
        perror("chdir rootfs");
        return false;
    }

    return true;
}

int main(int argc, char **argv)
{
    int sync_pipe[2];
    char sync_status = '.';
    int child_status;
    pid_t pid, parent_pid;

    if (pipe(sync_pipe) == -1) {
        perror("pipe");
        return 1;
    }

    parent_pid = getpid();

    switch (pid = fork()) {
        case -1:
            perror("fork");
            return 1;
        case 0:
            close(sync_pipe[1]);
            if (read(sync_pipe[0], &sync_status, 1) == -1) {
                perror("read pipe from parent");
                _exit(1);
            } else if (sync_status == 'X')
                _exit(1);
            close(sync_pipe[0]);
            _exit(write_maps(parent_pid) ? 0 : 1);
        default:
            if (unshare(CLONE_NEWNS | CLONE_NEWUSER) == -1) {
                perror("unshare");
                if (write(sync_pipe[1], "X", 1) == -1)
                    perror("signal child exit");
                waitpid(pid, NULL, 0);
                return 1;
            }

            close(sync_pipe[1]);
            waitpid(pid, &child_status, 0);
            if (WIFEXITED(child_status) && WEXITSTATUS(child_status) == 0)
                break;
            return 1;
    }

    if (!setup_chroot())
        return 1;

    argv[0] = WRAPPED_PROGNAME;
    if (execv(WRAPPED_PATH, argv) == -1) {
        fprintf(stderr, "exec %s: %s\n", WRAPPED_PATH, strerror(errno));
        return 1;
    }

    // Should never be reached.
    return 1;
}
