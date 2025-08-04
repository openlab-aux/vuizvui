#include <iostream>

#include <nix/store/local-fs-store.hh>
#include <nix/store/config.hh>
#include <nix/store/local-store.hh>
#include <nix/store/store-api.hh>

using namespace nix;

struct query_state {
    std::shared_ptr<Store> store;
    PathSet paths;
    PathSet::iterator iter;
};

static Path get_store_path(query_state *qs, Path path)
{
    Path canonicalized = canonPath(path, true);
    return qs->store->printStorePath(
        qs->store->toStorePath(canonicalized).first
    );
}

// Traverse the ancestors of the given path until we have a Nix store path.
//
// For example if the path is "/nix/store/...-foo/bar/1234"
// the result would be "/nix/store/...-foo".
static Path get_ancestor(query_state *qs, Path path)
{
    size_t pos = 0;
    std::string tmp;

    while (pos != std::string::npos) {
        if ((pos = path.find('/', pos + 1)) != std::string::npos) {
            Path current = path.substr(0, pos);

            if (!std::filesystem::is_symlink(current))
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
        initLibStore(false);
        initial->store = openStore();
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
            StorePathSet store_paths;

            qs->store->computeFSClosure(
                qs->store->followLinksToStorePath(query),
                store_paths,
                // flip direction
                false,
                // include outputs
                true
            );
            qs->paths = qs->store->printStorePathSet(store_paths);
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
