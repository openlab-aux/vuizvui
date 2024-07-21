{ pkgs, exactSource, haskellPackages }:
let

  # import the purescript file as nix expression via purenix.
  # Converts the purescript output to a nix file structure
  # puts it in the store and imports it.
  # Types are erased, functions are converted to nix functions,
  # unions values are TODO.
  importPurescript = { name, root, files, mainModule }:
    let
      src =
        exactSource
          root
          # exactSource wants nix paths, but I think relative paths
          # as strings are more intuitive.
          (let abs = path: toString root + "/" + path;
           in map abs files);

      mainPath = pkgs.lib.replaceStrings ["."] ["/"] mainModule;

      convert = pkgs.runCommandLocal "${name}-purs-to-nix" { } ''
        export LC_ALL=C.UTF-8
        ${haskellPackages.purescript}/bin/purs compile --codegen corefn '${src}/**/*.purs'
        # converts everything in the ./output dir to nix
        ${haskellPackages.purenix}/bin/purenix

        mkdir -p $out
        cp -r ./output/* $out
      '';
    in import "${convert}/${mainPath}";

in {
  inherit
    importPurescript
    ;
}
