{ pkgs, ... }:

with pkgs.lib;
with import ../lib;

{
  require = singleton ../common.nix;

  boot = let
    linuxAszlig = pkgs.linuxManualConfig {
      inherit (pkgs.kernelSourceAszlig) version src;

      configfile = generateKConf (import ./mmrnmhrm-kconf.nix);
      allowImportFromDerivation = true;
    };
  in rec {
    kernelPackages = pkgs.linuxPackagesFor linuxAszlig;

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

  services.xserver = {
    videoDrivers = [ "nouveau" ];

    xrandrHeads = [ "DVI-I-2" "DVI-I-1" ];

    displayManager.sessionCommands = ''
      ${pkgs.synergy}/bin/synergys -c "${../cfgfiles/synergy.conf}"
    '';
  };
}
