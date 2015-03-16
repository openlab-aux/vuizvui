with import <nixpkgs/lib>;

let
  genFromNixFiles = path: let
    isNixFile = name: type: hasSuffix ".nix" name && type == "regular";
    importNixFile = name: const {
      name = removeSuffix ".nix" name;
      value = import (builtins.toPath "${path}/${name}");
    };
  in mapAttrs' importNixFile (filterAttrs isNixFile (builtins.readDir path));

in {
  machines = mapAttrs (_: cfg: (import <nixpkgs/nixos/lib/eval-config.nix> {
    system = "x86_64-linux";
    modules = [ cfg ];
  }).config.system.build.toplevel) (genFromNixFiles ./machines);
}
