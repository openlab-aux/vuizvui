{ pkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [ "usbhid" ];
  boot.kernelModules = [ "kvm-intel" ];

  environment.systemPackages = with pkgs; [
    mosh wget krita rawtherapee darktable gphoto2 digikam
  ];

  fileSystems."/boot".device = "/dev/disk/by-uuid/A0D5-269D";
  fileSystems."/boot".fsType = "vfat";

  fileSystems."/".label = "tyree-root";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "compress=lzo"
    "discard"
    "noatime"
    "space_cache"
    "ssd"
  ];

  swapDevices = lib.singleton {
    label = "tyree-swap";
  };

  i18n.defaultLocale = "de_DE.UTF-8";

  networking.hostName = "tyree";
  networking.useNetworkd = true;

  nix.maxJobs = 4;

  # English within the shell, German otherwise (like in KDE).
  programs.bash.interactiveShellInit = lib.mkBefore ''
    export LANG=en_US.UTF-8
  '';

  services.xserver.xkbOptions = "eurosign:e,caps:none";
  services.xserver.wacom.enable = true;

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "bla";

  vuizvui.hardware.t100ha.enable = true;
}
