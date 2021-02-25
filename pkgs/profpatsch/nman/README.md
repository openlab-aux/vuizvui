# nman

nman is a tiny tool which allows you to view man pages for packages that are
not installed using the power of the nix package manager — which you'll
appreciate if you've ever tried to open a man page in a `nix-shell`.

Detailed documentation can be found in the `nman(1)` man page in this directory.

## installation

* with vuizvui: `pkgs.profpatsch.nman`
* without vuizvui: `nix-env -f https://github.com/openlab-aux/vuizvui/archive/master.tar.gz -iA pkgs.profpatsch.nman`
