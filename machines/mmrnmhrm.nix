{ pkgs, ... }:

with pkgs.lib;
with import ../lib;

{
  require = singleton ../common.nix;

  nix.maxJobs = 2;

  boot = let
    linuxAszlig = pkgs.buildLinux {
      inherit (pkgs.kernelSourceAszlig) version src;

      kernelPatches = singleton pkgs.aszligKernelPatches.bfqsched;
      configfile = generateKConf (import ./mmrnmhrm-kconf.nix);
      allowImportFromDerivation = true;
    };
  in rec {
    kernelParams = [
      "video=DVI-I-1:1600x1200"
      "video=VGA-1:1600x1200@60"
    ];

    kernelPackages = pkgs.recurseIntoAttrs
      (pkgs.linuxPackagesFor linuxAszlig kernelPackages);

    loader.grub.devices = singleton
      "/dev/disk/by-id/ata-WDC_WD6401AALS-00L3B2_WD-WMASY3263730";
  };

  networking.hostName = "mmrnmhrm";

  fileSystems = {
    "/" = {
      label = "root";
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
    label = "swap";
  };

  services.synergy.server.enable = true;
  services.synergy.server.configFile = ../cfgfiles/synergy.conf;

  services.xserver.videoDrivers = [ "nouveau" ];
  services.xserver.xrandrHeads = [ "DVI-I-1" "VGA-1" ];
}
