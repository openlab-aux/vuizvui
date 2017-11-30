#include <set>
#include <string>

typedef std::set<std::string> *path_cache;

extern "C" {
    path_cache new_path_cache(void)
    {
        return new std::set<std::string>();
    }

    void free_path_cache(path_cache pc)
    {
        delete pc;
    }

    bool cache_path(path_cache pc, const char *path)
    {
        return pc->insert(std::string(path)).second;
    }
}
