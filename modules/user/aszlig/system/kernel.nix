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
    version = "4.0.0-rc3";
    src = pkgs.fetchgit {
      url = git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git;
      rev = "affb8172de395a6e1db52ed9790ca0456d8c29a9";
      sha256 = "1zqnkds0mglldm1syv17gl8n4wl1vy7rrh2vf3iga5w2psqwkyj4";
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
