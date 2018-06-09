{ config, lib, pkgs, ... }:

let
  cfg = config.gog;

  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgs.pkgsi686Linux // self);

    fetchGog = callPackage ./fetch-gog {
      inherit (config.gog) email password;
    };

    albion = callPackage_i686 ./albion { inherit (pkgs) buildSandbox; };
    stardew-valley = callPackage ./stardew-valley.nix {};
    stardew-valley-beta = lib.lowPrio (callPackage ./stardew-valley.nix {
      beta = true;
    });
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
