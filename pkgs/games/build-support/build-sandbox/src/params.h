#ifndef _PARAMS_H
#define _PARAMS_H

#include <stdbool.h>
#include "nix-query.h"

bool setup_app_paths(void);
bool mount_runtime_path_vars(struct query_state *qs);

#endif
