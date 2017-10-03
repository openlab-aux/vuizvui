#include <iostream>

#if NIX_VERSION >= 112
#include <nix/config.h>
#endif
#include <nix/util.hh>
#include <nix/local-store.hh>
#include <nix/store-api.hh>

#if NIX_VERSION < 112
#include <nix/misc.hh>
#include <nix/globals.hh>
#endif

using namespace nix;

struct query_state {
#if NIX_VERSION >= 112
    std::shared_ptr<Store> store;
#else
    std::shared_ptr<StoreAPI> store;
#endif
    PathSet paths;
    PathSet::iterator iter;
};

static Path get_store_path(query_state *qs, Path path)
{
    Path canonicalized = canonPath(path, true);
#if NIX_VERSION >= 112
    return qs->store->toStorePath(canonicalized);
#else
    return toStorePath(canonicalized);
#endif
}

static Path get_ancestor(query_state *qs, Path path)
{
    size_t pos = 0;
    std::string tmp;

    while (pos != std::string::npos) {
        if ((pos = path.find('/', pos + 1)) != std::string::npos) {
            Path current = path.substr(0, pos);

            if (!isLink(current))
                continue;

            try {
                current = get_store_path(qs, current);
            } catch (...) {
                continue;
            }

            return current;
        }
    }

    return get_store_path(qs, path);
}

extern "C" {
    struct query_state *new_query(void)
    {
        query_state *initial = new query_state();
#if NIX_VERSION >= 112
        initial->store = openStore();
#else
        settings.processEnvironment();
        settings.loadConfFile();
        initial->store = openStore(false);
#endif
        return initial;
    }

    void free_query(query_state *qs)
    {
        delete qs;
    }

    bool query_requisites(query_state *qs, const char *path)
    {
        Path query(path);

        try {
            query = get_ancestor(qs, query);

#if NIX_VERSION >= 112
            qs->store->computeFSClosure(
                qs->store->followLinksToStorePath(query),
                qs->paths, false, true
            );
#else
            computeFSClosure(
                *qs->store, followLinksToStorePath(query),
                qs->paths, false, true
            );
#endif
        } catch (Error &e) {
            std::cerr << "Error while querying requisites for "
                      << query << ": " << e.what()
                      << std::endl;
            return false;
        }

        qs->iter = qs->paths.begin();

        return true;
    }

    const char *next_query_result(query_state *qs)
    {
        if (qs->iter == qs->paths.end())
            return NULL;

        return (qs->iter++)->c_str();
    }
}
