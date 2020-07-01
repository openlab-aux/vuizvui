{ config, lib, pkgs, ... }:

let
  cfg = config.gog;

  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgs.pkgsi686Linux // self);

    fetchGog = callPackage ./fetch-gog {
      inherit (config.gog) email password;
    };

    albion = callPackage_i686 ./albion {};
    baldurs-gate = (callPackage ./baldurs-gate {}).bg1ee;
    baldurs-gate-2 = (callPackage ./baldurs-gate {}).bg2ee;
    crosscode = callPackage ./crosscode.nix {};
    dungeons3 = callPackage ./dungeons3.nix {};
    epistory = callPackage ./epistory.nix { };
    freedom-planet = callPackage ./freedom-planet.nix {};
    gibbous = callPackage ./gibbous.nix {};
    homm3 = callPackage ./homm3 {};
    hollow-knight = callPackage ./hollow-knight.nix {};
    into-the-breach = callPackage ./into-the-breach.nix {};
    kingdoms-and-castles = callPackage ./kingdoms-and-castles.nix {};
    knights-and-bikes = callPackage ./knights-and-bikes.nix {};
    overload = callPackage ./overload.nix {};
    party-hard = callPackage ./party-hard.nix {};
    planescape-torment-enhanced-edition = callPackage ./planescape-torment-enhanced-edition.nix {};
    satellite-reign = callPackage ./satellite-reign.nix {};
    settlers2 = callPackage ./settlers2.nix {};
    stardew-valley = callPackage ./stardew-valley.nix {};
    sunless-skies = callPackage ./sunless-skies.nix {};
    the-longest-journey = callPackage ./the-longest-journey {};
    thimbleweed-park = callPackage ./thimbleweed-park.nix {};
    war-for-the-overworld = callPackage ./war-for-the-overworld.nix {};
    warcraft2 = callPackage ./warcraft2 {};
    wizard-of-legend = callPackage ./wizard-of-legend.nix {};
    xeen = callPackage ./xeen.nix {};
  };
in {
  options.gog = {
    email = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Email address for your GOG account.
      '';
    };

    password = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Password for your GOG account.

        <note><para>This will end up in the Nix store and other users on the
        same machine can read it!</para></note>
      '';
    };
  };

  config.packages = {
    gog = lib.mkIf (cfg.email != null && cfg.password != null) self;
  };
}
