{ pkgs, lib, ... }:

{
  vuizvui.user.aszlig.profiles.workstation.enable = true;

  vuizvui.system.kernel.bfq.enable = true;

  boot = {
    loader.systemd-boot.enable = true;
    loader.grub.enable = lib.mkForce false;
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;

    initrd = {
      luks.devices = {
        slylandro-swap = {
          device = "/dev/disk/by-uuid/6ce7f3a4-64cc-4d5c-a220-662d3d589220";
        };
        slylandro-root = {
          device = "/dev/disk/by-uuid/76ddcc8f-060e-44f9-af08-886d7395e466";
        };
      };
    };
  };

  networking.hostName = "slylandro";
  networking.wireless.enable = lib.mkForce true;
  networking.interfaces.enp5s0.useDHCP = true;
  networking.interfaces.wlp3s0.useDHCP = true;

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/DF3B-267A";
    fsType = "vfat";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a96de4ae-60a3-463d-bca3-2ef7f207f8f4";
    fsType = "btrfs";
    options = [ "space_cache" "compress=zstd" "noatime" ];
  };

  swapDevices = lib.singleton {
    device = "/dev/disk/by-uuid/9c5a118d-656a-4bf5-833a-225bb2c887c4";
  };

  services.tlp.enable = true;

  services.xserver.videoDrivers = [ "nouveau" "intel" ];
  services.xserver.libinput.enable = true;

  nix.maxJobs = 4;
}
