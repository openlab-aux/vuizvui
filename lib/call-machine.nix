path: cfg:

let
  # Extract optional module selection from cfg
  # If cfg has _moduleSelection, use selectModules; otherwise use all modules
  moduleSelection = cfg._moduleSelection or null;
  selectModules = import ./select-modules.nix;
  selectedModules =
    if moduleSelection != null
    then selectModules moduleSelection
    else import ../modules/module-list.nix;

  # Remove _moduleSelection from cfg before passing to modules
  cleanCfg = builtins.removeAttrs cfg [ "_moduleSelection" ];

  __withPkgsPath = nixpkgs: rec {
    eval = import "${nixpkgs}/nixos/lib/eval-config.nix" {
      modules = [ path cleanCfg ] ++ selectedModules;
    };

    build = eval.config.system.build.toplevel;

    iso = mkIso "installer/cd-dvd/iso-image.nix" (
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

    installerIso = mkIso "installer/cd-dvd/installation-cd-minimal.nix" {
      environment.sessionVariables = {
        NIX_PATH = [ "vuizvui=${../.}" ];
      };
    };

    mkIso = isoModule: extraConfig: let
      wrapIso = { config, pkgs, lib, ... }@attrs: let
        isoEval = import "${nixpkgs}/nixos/modules/${isoModule}" attrs;
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
      modules = [ config wrapIso ];
    };

    config = {
      imports = [ path cleanCfg ] ++ selectedModules;
    };

    vm = (import "${nixpkgs}/nixos" {
      configuration = config;
    }).vm;
  };

in rec {
  inherit (__withPkgsPath (import ../nixpkgs-path.nix))
    build config eval iso installerIso vm;

  # This is internal only and for use with restricted evaluation mode in Hydra
  # to get the path to nixpkgs from the jobset input args instead of
  # ../nixpkgs-path.nix.
  inherit __withPkgsPath;
}
