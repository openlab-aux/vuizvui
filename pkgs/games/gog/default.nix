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
    dungeons3 = callPackage ./dungeons3.nix {};
    epistory = callPackage ./epistory.nix { };
    overload = callPackage ./overload.nix {};
    party-hard = callPackage ./party-hard.nix {};
    satellite-reign = callPackage ./satellite-reign.nix {};
    settlers2 = callPackage ./settlers2.nix {};
    stardew-valley = callPackage ./stardew-valley.nix {};
    thimbleweed-park = callPackage ./thimbleweed-park.nix {};
    war-for-the-overworld = callPackage ./war-for-the-overworld.nix {};
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
