{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.system.kernel;

  generateKConf = exprs: let
    isNumber = c: elem c ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"];
    mkValue = val:
      if val == "" then "\"\""
      else if val == "y" || val == "m" || val == "n" then val
      else if all isNumber (stringToCharacters val) then val
      else if substring 0 2 val == "0x" then val
      else "\"${val}\"";
    mkConfigLine = key: val: "${key}=${mkValue val}";
    mkConf = cfg: concatStringsSep "\n" (mapAttrsToList mkConfigLine cfg);
  in pkgs.writeText "generated.kconf" (mkConf exprs + "\n");

  mainlineKernel = {
    version = "4.4.0-rc4";
    src = pkgs.fetchgit {
      url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
      rev = "62ea1ec5e17fe36e2c728bc534f9f78b216dfe83";
      sha256 = "1gy8zcrqrrba21hz63bxxp2lg3wkj839lmj4p6yc3mmcg8xrn2gn";
    };
  };

in {
  options.vuizvui.user.aszlig.system.kernel = {
    enable = mkEnableOption "aszlig's custom kernel";

    config = mkOption {
      type = types.attrsOf types.unspecified;
      default = {};
      description = ''
        An attribute set of configuration options to use
        for building a custom kernel.
      '';
    };
  };

  config = mkIf cfg.enable {
    boot = let
      linuxVuizvui = pkgs.buildLinux {
        inherit (mainlineKernel) version src;

        kernelPatches = singleton pkgs.vuizvui.kernelPatches.bfqsched;
        configfile = generateKConf cfg.config;
        allowImportFromDerivation = true;
      };
    in rec {
      kernelPackages = pkgs.recurseIntoAttrs
        (pkgs.linuxPackagesFor linuxVuizvui kernelPackages);
    };
  };
}
