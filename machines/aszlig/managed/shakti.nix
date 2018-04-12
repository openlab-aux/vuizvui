# FIXME: Currently just a placeholder to get Hydra builds.
{ pkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [ "usbhid" ];
  boot.kernelModules = [ "kvm-amd" ];

  environment.systemPackages = with pkgs; [
    mosh wget krita gphoto2 digikam
  ];

  # TODO: fileSystems."/boot".device = "/dev/disk/by-uuid/XXX";
  # TODO: fileSystems."/boot".fsType = "vfat";

  fileSystems."/".label = "shakti-root";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "compress=zstd"
    "noatime"
    "space_cache"
  ];

  swapDevices = lib.singleton {
    label = "tyree-swap";
  };

  networking.hostName = "shakti";
  networking.useNetworkd = true;

  hardware.cpu.amd.updateMicrocode = true;

  nix.maxJobs = 4;

  services.xserver.xkbOptions = "eurosign:e,caps:none";
  services.xserver.libinput.enable = true;

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "aortab";
}
