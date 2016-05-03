path:

{ system ? builtins.currentSystem
, extraConfig ? {}
}:

let
  nixpkgs = import ../nixpkgs-path.nix;

  eval = import "${nixpkgs}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [ path extraConfig ] ++ import ../modules/module-list.nix;
  };

  iso = mkIso "${nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix" (
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
  );

  installerIso = mkIso "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix" {
    environment.sessionVariables = {
      NIX_PATH = [ "vuizvui=${../.}" ];
    };
  };

  mkIso = isoModule: extraConfig: let
    wrapIso = { config, pkgs, lib, ... }@attrs: let
      isoEval = import isoModule attrs;
      isoEvalcfg = isoEval.config or {};
      bootcfg = isoEvalcfg.boot or {};
      fscfg = isoEvalcfg.fileSystems or {};
    in {
      options = isoEval.options or {};
      imports = (isoEval.imports or []) ++ [ extraConfig ];
      config = isoEvalcfg // {
        boot = bootcfg // lib.optionalAttrs (bootcfg ? loader) {
          loader = lib.mkForce bootcfg.loader;
        };
        fileSystems = lib.mapAttrs (lib.const lib.mkForce) fscfg // {
          "/boot" = lib.mkForce (fscfg."/boot" or {
            device = "none";
            fsType = "none";
            options = [ "noauto" ];
          });
        };
      };
    };
  in import "${nixpkgs}/nixos/lib/eval-config.nix" {
    inherit system;
    modules = [ config wrapIso ];
  };

  config = {
    imports = [ path extraConfig ] ++ import ../modules/module-list.nix;
  };

  vm = (import "${nixpkgs}/nixos" {
    inherit system;
    configuration = config;
  }).vm;

in {
  build = eval.config.system.build.toplevel;
  inherit config eval iso installerIso vm;
}
