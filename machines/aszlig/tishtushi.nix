{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  vuizvui.system.kernel.bfq.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader = {
    grub.enable = true;
    grub.device = "/dev/disk/by-id/ata-Hitachi_HTS543232A7A384_E2P31243FGB6PJ";
    timeout = 1;
  };

  boot.initrd = {
    luks.devices = {
      "00vault" = {
        device = "/dev/disk/by-uuid/812f19f1-9096-4367-b2e4-0c9537c52a67";
      };
      tishtushi-swap = {
        device = "/dev/disk/by-uuid/2934df87-5fda-4b2e-9f3b-c4c96f571407";
        keyFile = "/dev/mapper/00vault";
      };
      tishtushi-root = {
        device = "/dev/disk/by-uuid/cf65f144-9205-40a5-a239-b660695a6740";
        keyFile = "/dev/mapper/00vault";
      };
    };
    postDeviceCommands = lib.mkAfter ''
      cryptsetup luksClose /dev/mapper/00vault
    '';
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
    options = [ "space_cache" "compress=zstd" "noatime" ];
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
