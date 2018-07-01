{ config, lib, pkgs, ... }:

let
  cfg = config.itch;

  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);
    callPackage_i686 = pkgs.lib.callPackageWith (pkgs.pkgsi686Linux // self);

    fetchItch = callPackage ./fetch-itch {
      inherit (config.itch) apiKey;
    };

    invisigun-heroes = callPackage ./invisigun-heroes.nix {};
    towerfall-ascension = callPackage ./towerfall-ascension.nix {};
  };
in {
  options.itch.apiKey = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = ''
      The API key of your <link xlink:href="https://itch.io/">itch.io</link>
      account, can be retrieved by heading to <link
      xlink:href="https://itch.io/user/settings/api-keys"/>.
    '';
  };

  config.packages.itch = lib.mkIf (cfg.apiKey != null) self;
}
