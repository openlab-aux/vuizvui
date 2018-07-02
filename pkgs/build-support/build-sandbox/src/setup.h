#ifndef _SETUP_H
#define _SETUP_H

#include <stdbool.h>
#include <sys/types.h>
#include "nix-query.h"

char *get_mount_target(const char *path);
bool write_maps(pid_t parent_pid);
bool bind_mount(const char *path, bool rdonly, bool restricted, bool resolve);
bool extra_mount(const char *path, bool is_required);
bool mount_from_path_var(struct query_state *qs, const char *name);
bool setup_sandbox(void);

#endif
