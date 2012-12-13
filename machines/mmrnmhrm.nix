{ config, pkgs, ... }:

with pkgs.lib;

{
  require = singleton ../common.nix;

  boot = let
    linuxAszlig = pkgs.linuxManualConfig {
      version = pkgs.kernelSourceAszlig.version;
      src = pkgs.kernelSourceAszlig.src;

      configfile = let
        isNumber = c: elem c ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"];
        mkValue = val:
          if val == "" then "\"\""
          else if val == "y" || val == "m" || val == "n" then val
          else if all isNumber (stringToCharacters val) then val
          else if substring 0 2 val == "0x" then val
          else "\"${val}\"";
        mkConfigLine = key: val: "${key}=${mkValue val}";
        mkConf = cfg: concatStringsSep "\n" (mapAttrsToList mkConfigLine cfg);
      in pkgs.writeText "aszlig.kconf" (mkConf (import ./mmrnmhrm-kconf.nix));

      allowImportFromDerivation = true;
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig kernelPackages;

    initrd = {
      luks.enable = true;
      luks.devices = [
        { name = "system_crypt";
          device = "/dev/disk/by-uuid/91fbc437-b191-4c32-add4-6f96b2c953dd";
          preLVM = true;
        }
      ];
    };

    loader.grub.devices = [
      "/dev/disk/by-id/ata-WDC_WD10EAVS-00D7B1_WD-WCAU48931237"
    ];
  };

  networking.hostName = "mmrnmhrm";

  fileSystems = {
    "/boot" = {
      label = "boot";
      fsType = "ext2";
    };
    "/" = {
      device = "/dev/system/root";
      fsType = "btrfs";
      options = concatStringsSep "," [
        "autodefrag"
        "space_cache"
        "inode_cache"
        "compress=lzo"
        "noatime"
      ];
    };
  };

  swapDevices = singleton {
    device = "/dev/system/swap";
  };

  services.xserver.videoDrivers = [ "nouveau" ];
}
