# simplified version of build-dhall-package.nix in nixpkgs
# In particular, this keeps the full dhall cache instead of
# just the outermost cache of the fully evaluated package.
# That makes it possible to e.g. just import one function from
# the dhall prelude instead of the whole Prelude, which speeds up
# evaluation considerably (since dhall evaluation is not lazy).
{ haskell, dhall, lib, lndir, runCommand, writeText }:

{ name

  # Expressions to add to the cache before interpreting the code
, dependencies ? []

  # A Dhall expression
  #
  # Carefully note that the following expression must be devoid of uncached HTTP
  # imports.  This is because the expression will be evaluated using an
  # interpreter with HTTP support disabled, so all HTTP imports have to be
  # protected by an integrity check that can be satisfied via cached
  # dependencies.
  #
  # You can add a dependency to the cache using the preceding `dependencies`
  # option
, code

  # `buildDhallPackage` can include both a "source distribution" in
  # `source.dhall` and a "binary distribution" in `binary.dhall`:
  #
  # * `source.dhall` is a dependency-free αβ-normalized Dhall expression
  #
  # * `binary.dhall` is an expression of the form: `missing sha256:${HASH}`
  #
  #   This expression requires you to install the cache product located at
  #   `.cache/dhall/1220${HASH}` to successfully resolve
  #
  # By default, `buildDhallPackage` only includes "binary.dhall" to conserve
  # space within the Nix store, but if you set the following `source` option to
  # `true` then the package will also include `source.dhall`.
, source ? false
}:

let

  file = writeText "${name}.dhall" code;

  cache = ".cache";

  cacheDhall = "${cache}/dhall";

in
  runCommand name { inherit dependencies; } ''
    set -eu

    mkdir -p $out/${cacheDhall}

    for dependency in $dependencies; do
      ${lndir}/bin/lndir -silent $dependency/${cacheDhall} $out/${cacheDhall}
    done

    export XDG_CACHE_HOME=$out/${cache}

    ${dhall}/bin/dhall --alpha --file '${file}' > alpha.dhall

    SHA_HASH=$(${dhall}/bin/dhall hash --file alpha.dhall)

    HASH_FILE="''${SHA_HASH/sha256:/1220}"

    ${dhall}/bin/dhall encode --file alpha.dhall > $out/${cacheDhall}/$HASH_FILE
  ''
