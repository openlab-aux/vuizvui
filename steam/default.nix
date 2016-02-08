{ config, lib, pkgs, ... }:

let
  cfg = config.steam;

  self = rec {
    callPackage = pkgs.lib.callPackageWith (pkgs // self);

    fetchSteam = callPackage ./fetchsteam {
      inherit (config.steam) username password;
    };

    starbound = callPackage ./starbound.nix { flavor = "stable"; };
    starbound-unstable = callPackage ./starbound.nix { flavor = "unstable"; };
  };
in with lib; {
  options.steam = {
    username = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        User name for your Steam account.
      '';
    };

    password = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Password for your Steam account.
      '';
    };
  };

  config.packages = {
    steam = mkIf (cfg.username != null && cfg.password != null) self;
  };
}
