#include <nix/util.hh>
#include <nix/local-store.hh>
#include <nix/store-api.hh>
#include <nix/misc.hh>

using namespace nix;

int get_closure(const char *path)
{
    Path query(path);
    PathSet paths;
    auto store = openStore(false);

    computeFSClosure(
        *store, followLinksToStorePath(query), paths, false, true
    );

    for (auto i = paths.begin(); i != paths.end(); ++i) {
        // TODO!
    }

    return 1;
}
