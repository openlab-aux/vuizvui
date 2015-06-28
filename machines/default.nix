{ system ? builtins.currentSystem, ... }:

let
  nixpkgs = import ../nixpkgs-path.nix;

  callMachine = path: rec {
    config = import path;
    build = import "${nixpkgs}/nixos/lib/eval-config.nix" {
      inherit system;
      modules = [ config ] ++ import ../modules/module-list.nix;
    };
    iso = import "${nixpkgs}/nixos/lib/eval-config.nix" {
      inherit system;
      modules = [
        { options = { inherit (build.options) vuizvui; };
          config = (builtins.removeAttrs build.config [
            "_module" "boot" "fileSystems"
          ]) // {
            boot = builtins.removeAttrs build.config.boot [ "loader" ];
          };
        }
        "${nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix"
        (
          { lib, ... }: let
            name = build.config.networking.hostName;
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
    use = {
      imports = [ config ] ++ import ../modules/module-list.nix;
    };
  };

in {
  aszlig = {
    dnyarri   = callMachine ./aszlig/dnyarri.nix;
    mmrnmhrm  = callMachine ./aszlig/mmrnmhrm.nix;
    arilou    = callMachine ./aszlig/arilou.nix;
    kzerza    = callMachine ./aszlig/kzerza.nix;
    tishtushi = callMachine ./aszlig/tishtushi.nix;
  };
  labnet = {
    heinrich = callMachine ./labnet/heinrich.nix;
    labtop   = callMachine ./labnet/labtop.nix;
  };
  profpatsch = {
    katara = callMachine ./profpatsch/katara.nix;
  };
  misc = {
    mailserver = callMachine ./misc/mailserver.nix;
  };
}
