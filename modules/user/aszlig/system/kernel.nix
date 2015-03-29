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
    version = "4.0.0-rc5";
    src = pkgs.fetchgit {
      url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
      rev = "7fc377ecf452c02f3fd6aa0db9a3fd7d012ff500";
      sha256 = "1cgwhhrqi4h2azf5cw6x7rizmzschzziq93wx8b5ld50sh4rwg4v";
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

      loader.grub.devices = map (i: "/dev/disk/by-id/${i}") [
        "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5756955"
        "ata-WDC_WD10EZEX-00BN5A0_WD-WCC3F5790537"
      ];
    };
  };
}
