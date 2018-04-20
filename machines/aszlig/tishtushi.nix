{ lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  vuizvui.system.kernel.bfq.enable = true;
  vuizvui.system.kernel.useBleedingEdge = true;

  boot.loader = {
    grub.device = "/dev/disk/by-id/ata-Hitachi_HTS543232A7A384_E2P31243FGB6PJ";
    timeout = 1;
  };

  boot.initrd = {
    kernelModules = [ "fbcon" "usb_storage" ];
    availableKernelModules = [
      "aes_x86_64" "af_alg" "algif_skcipher" "cbc" "cryptd" "crypto_simd"
      "dm_crypt" "ecb" "gf128mul" "glue_helper" "xts"
    ];
    luks.devices = [
      { name = "00vault";
        device = "/dev/disk/by-uuid/812f19f1-9096-4367-b2e4-0c9537c52a67";
      }
      { name = "tishtushi-swap";
        device = "/dev/disk/by-uuid/2934df87-5fda-4b2e-9f3b-c4c96f571407";
        keyFile = "/dev/mapper/00vault";
      }
      { name = "tishtushi-root";
        device = "/dev/disk/by-uuid/cf65f144-9205-40a5-a239-b660695a6740";
        keyFile = "/dev/mapper/00vault";
      }
      { name = "tishtushi-nix";
        device = "/dev/disk/by-uuid/af7fc49a-cc38-49f2-8a89-1cd8248554a7";
        keyFile = "/dev/mapper/00vault";
      }
    ];
    postDeviceCommands = lib.mkAfter ''
      cryptsetup luksClose /dev/mapper/00vault
    '';
  };

  networking.hostName = "tishtushi";
  networking.wireless.enable = lib.mkForce true;

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/763a7117-3dbf-4e80-9e63-c7039477ef3d";
    fsType = "ext4";
  };

  fileSystems."/" = {
    device = "/dev/mapper/tishtushi-root";
    fsType = "btrfs";
    options = [ "space_cache" "compress=zstd" "noatime" ];
  };

  fileSystems."/nix" = {
    device = "/dev/mapper/tishtushi-nix";
    fsType = "btrfs";
    options = [ "ssd" "compress=zstd" "noatime" ];
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
