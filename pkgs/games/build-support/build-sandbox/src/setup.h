#ifndef _SETUP_H
#define _SETUP_H

#include <stdbool.h>
#include <sys/types.h>

bool write_maps(pid_t parent_pid);
bool bind_mount(const char *path, bool restricted, bool resolve);
bool extra_mount(const char *path);
bool setup_sandbox(void);

#endif
