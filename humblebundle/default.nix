{ config, lib, pkgs, ... }:

let
  cfg = config.humblebundle;

  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgs.pkgsi686Linux // self);

    fetchHumbleBundle = callPackage ./fetch-humble-bundle {
      inherit (config.humblebundle) email password;
    };

    bastion = callPackage ./bastion.nix {};
    cavestoryplus = callPackage ./cavestoryplus.nix {};
    fez = callPackage ./fez.nix {};
    ftl = callPackage ./ftl.nix {};
    guacamelee = callPackage_i686 ./guacamelee.nix {};
    hammerwatch = callPackage ./hammerwatch.nix {};
    jamestown = callPackage ./jamestown.nix {};
    liads = callPackage ./liads.nix {};
    megabytepunch = callPackage ./megabytepunch.nix {};
    rocketbirds = callPackage ./rocketbirds.nix {};
    spaz = callPackage ./spaz.nix {};
    swordsandsoldiers = callPackage ./swordsandsoldiers.nix {};
  };
in with lib; {
  options.humblebundle = {
    email = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Email address for your HumbleBundle account.
      '';
    };

    password = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Password for your HumbleBundle account.
      '';
    };
  };

  config.packages = {
    humblebundle = mkIf (cfg.email != null && cfg.password != null) self;
  };
}
