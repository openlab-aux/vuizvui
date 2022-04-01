{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    grub.enable = true;
    grub.device =
      "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_500GB_S3Z2NB0KA77896M";
    timeout = 1;
  };

  boot.initrd.availableKernelModules = [ "xxhash_generic" ];
  boot.initrd.luks.devices = {
    tishtushi-swap = {
      device = "/dev/disk/by-uuid/db144d14-9268-4f12-a421-e9c41fc207a3";
    };
    tishtushi-root = {
      device = "/dev/disk/by-uuid/e8eafbdd-6056-41c7-88bc-51bfb8a98e22";
    };
  };

  hardware.cpu.intel.updateMicrocode = true;

  networking.hostName = "tishtushi";
  networking.wireless.enable = lib.mkForce true;
  networking.interfaces.wlp2s0.useDHCP = true;

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/763a7117-3dbf-4e80-9e63-c7039477ef3d";
    fsType = "ext4";
  };

  fileSystems."/" = {
    device = "/dev/mapper/tishtushi-root";
    fsType = "btrfs";
    options = [ "space_cache=v2" "compress=zstd" "noatime" "discard=async" ];
  };

  swapDevices = lib.singleton {
    device = "/dev/mapper/tishtushi-swap";
  };

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.tapButtons = true;
  services.xserver.synaptics.twoFingerScroll = true;
  services.xserver.synaptics.vertEdgeScroll = false;
  services.xserver.synaptics.accelFactor = "0.1";

  nix.maxJobs = 4;
}
