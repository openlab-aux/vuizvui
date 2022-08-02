{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  boot = {
    loader.systemd-boot.enable = true;
    loader.grub.enable = false;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" ];
      luks.devices = {
        slylandro-swap = {
          device = "/dev/disk/by-uuid/30904e53-cc8f-4039-9244-fc648dc6f801";
        };
        slylandro-root = {
          device = "/dev/disk/by-uuid/4b63a017-796b-4608-a6eb-85a47f894d30";
        };
      };
    };
  };

  hardware.cpu.amd.updateMicrocode = true;
  hardware.video.hidpi.enable = true;

  networking.hostName = "slylandro";
  networking.wireless.enable = lib.mkForce true;
  networking.interfaces.enp1s0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/DF50-CD44";
    fsType = "vfat";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/87a842a8-e2c5-45b6-8e67-58fec33b5eee";
    fsType = "btrfs";
    options = [
      "autodefrag" "space_cache=v2" "compress=zstd" "noatime" "discard=async"
    ];
  };

  swapDevices = lib.singleton {
    device = "/dev/disk/by-uuid/feace1d7-e48d-4b7e-a601-1515f9b90c20";
  };

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.libinput.enable = true;

  nix.maxJobs = 16;
}
