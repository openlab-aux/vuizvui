#ifndef _PATH_CACHE_H
#define _PATH_CACHE_H

typedef void *path_cache;

path_cache new_path_cache(void);
void free_path_cache(path_cache pc);
bool cache_path(path_cache pc, const char *path);

#endif
