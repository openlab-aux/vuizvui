{ pkgs, dhall-nix, dhall-json, exactSource }:
let

  # import the dhall file as nix expression via dhall-nix.
  # Converts the normalized dhall expression to a nix file,
  # puts it in the store and imports it.
  # Types are erased, functions are converted to nix functions,
  # unions values are nix functions that take a record of match
  # functions for their alternatives.
  importDhall = dhallType: file: importDhall2 {
    root = builtins.dirOf file;
    files = [];
    main = builtins.baseNameOf file;
    type = dhallType;
    deps = [];
  };

  # TODO: document
  importDhall2 = { root, files, main, deps, type }:
    let
      src =
        exactSource
          root
          # exactSource wants nix paths, but I think relative paths
          # as strings are more intuitive.
          (let abs = path: toString root + "/" + path;
           in ([ (abs main) ] ++ (map abs files)));

      cache = ".cache";
      cacheDhall = "${cache}/dhall";

      convert = pkgs.runCommandLocal "dhall-to-nix" { inherit deps; } ''
        mkdir -p ${cacheDhall}
        for dep in $deps; do
          ${pkgs.xorg.lndir}/bin/lndir -silent $dep/${cacheDhall} ${cacheDhall}
        done

        export XDG_CACHE_HOME=./${cache}
        printf '%s' ${pkgs.lib.escapeShellArg "${src}/${main} : ${type}"} \
          | ${dhall-nix}/bin/dhall-to-nix \
          > $out
      '';
    in import convert;


  # read dhall file in as JSON, then import as nix expression.
  # The dhall file must not try to import from non-local URLs!
  readDhallFileAsJson = dhallType: file:
    let
      convert = pkgs.runCommandLocal "dhall-to-json" {} ''
        printf '%s' ${pkgs.lib.escapeShellArg "${file} : ${dhallType}"} \
          | ${dhall-json}/bin/dhall-to-json \
          > $out
      '';
    in builtins.fromJSON (builtins.readFile convert);

in {
  inherit
    importDhall
    importDhall2
    readDhallFileAsJson
    ;
}
