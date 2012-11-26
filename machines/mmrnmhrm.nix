{config, pkgs, ...}:

with pkgs.lib;

{
  require = singleton ../common.nix;

  boot = let
    linuxAszlig = pkgs.linuxManualConfig {
      version = "3.7.0-rc6";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "c56dcc86b9a0a140ae0b35abb4b2ecd1b45e8bda";
        sha256 = "1km8dgfgdcgngcdnj5jzy98zyn7mrfryygnrp2wvzk5vi53wksmx";
      };
      configfile = pkgs.fetchurl {
        name = "aszlig.kconf";
        url = "file:///home/aszlig/linux/.config";
        md5 = "0c632194689797846127b47fa135c516";
      };
      allowImportFromDerivation = true; # XXX
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
