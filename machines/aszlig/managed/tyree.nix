{ pkgs, lib, ... }:

{
  boot.initrd.availableKernelModules = [ "usbhid" ];
  boot.kernelModules = [ "kvm-intel" ];

  boot.initrd.luks.devices = [
    { name = "00-vault";
      device = "/dev/disk/by-uuid/e4eb3d30-7fa5-4af4-86fb-80b47518cc25";
    }
    { name = "tyree-swap";
      device = "/dev/disk/by-uuid/d96e29b4-0b9a-442d-af27-805f69ffffb3";
      keyFile = "/dev/mapper/00-vault";
    }
    { name = "tyree-root";
      device = "/dev/disk/by-uuid/21e9a86e-c8dc-4d8f-ba75-d03552dc32f7";
      keyFile = "/dev/mapper/00-vault";
    }
  ];

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    cryptsetup luksClose /dev/mapper/00-vault
  '';

  environment.systemPackages = with pkgs; [
    aqbanking darktable digikam firefox gwenhywfar gphoto2 kgpg kmymoney krita
    mosh python2Packages.weboob rawtherapee wget
  ];

  fileSystems."/boot".device = "/dev/disk/by-uuid/A0D5-269D";
  fileSystems."/boot".fsType = "vfat";

  fileSystems."/".label = "tyree-root";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "compress=zstd"
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
  networking.useDHCP = false;
  networking.interfaces.wlan0.useDHCP = true;

  hardware.cpu.intel.updateMicrocode = true;

  nix.maxJobs = 4;

  # English within the shell, German otherwise (like in KDE).
  programs.bash.interactiveShellInit = lib.mkBefore ''
    export LANG=en_US.UTF-8
  '';

  services.journald.extraConfig = "SystemMaxUse=100M";

  services.xserver.xkbOptions = "eurosign:e,caps:none";
  services.xserver.wacom.enable = true;

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "bla";

  vuizvui.hardware.t100ha.enable = true;
}
