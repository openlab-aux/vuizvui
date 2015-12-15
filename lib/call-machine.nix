path: { system ? builtins.currentSystem }:

let
  nixpkgs = import ../nixpkgs-path.nix;

  eval = import "${nixpkgs}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [ path ] ++ import ../modules/module-list.nix;
  };

  iso = let
    isoModule = "${nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix";
    patchedModule = (import nixpkgs {}).runCommand "iso-image.nix" {} ''
      sed -e 's|../../../lib/|${nixpkgs}/nixos/lib/|g' \
          -e 's/"nomodeset"//g' \
          "${isoModule}" > "$out"
    '';
    wrapIso = { config, pkgs, lib, ... }@attrs: let
      patchedEval = (import patchedModule attrs);
      patchedEvalcfg = patchedEval.config or {};
      bootcfg = patchedEvalcfg.boot or {};
      fscfg = patchedEvalcfg.fileSystems or {};
    in {
      options = patchedEval.options or {};
      imports = patchedEval.imports or [];
      config = patchedEvalcfg // {
        boot = bootcfg // lib.optionalAttrs (bootcfg ? loader) {
          loader = lib.mkForce bootcfg.loader;
        };
        fileSystems = lib.mapAttrs (lib.const lib.mkForce) fscfg // {
          "/boot" = lib.mkForce (fscfg."/boot" or {
            device = "none";
            fsType = "none";
            options = "noauto";
          });
        };
      };
    };
  in import "${nixpkgs}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [
      config wrapIso
      (
        { lib, ... }: let
          name = eval.config.networking.hostName;
          upperName = lib.toUpper name;
        in rec {
          isoImage.isoName = "${name}.iso";
          isoImage.volumeID = builtins.substring 0 11 "${upperName}_LIVE";
          isoImage.makeEfiBootable = true;
          isoImage.makeUsbBootable = true;
          isoImage.appendToMenuLabel = " \"${name}\" Live System";
        }
      )
    ];
  };

  config = {
    imports = [ path ] ++ import ../modules/module-list.nix;
  };

  vm = (import "${nixpkgs}/nixos" {
    inherit system;
    configuration = config;
  }).vm;

in {
  inherit config eval iso vm;
}
