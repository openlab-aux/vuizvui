{ pkgs, unfreeAndNonDistributablePkgs, lib, ... }:

{
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [
    { name = "00vault";
      device = "/dev/disk/by-uuid/a70f4ff8-e463-42fa-8148-6783dd352f96";
    }
    { name = "shakti-swap";
      device = "/dev/disk/by-uuid/69f3a774-c796-4dbd-a38b-32f019d05e7c";
      keyFile = "/dev/mapper/00vault";
    }
    { name = "shakti-root";
      device = "/dev/disk/by-uuid/8a67bdf9-08bb-4214-b728-88cf1c2ee206";
      keyFile = "/dev/mapper/00vault";
    }
  ];
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    cryptsetup luksClose /dev/mapper/00vault
  '';

  boot.kernelModules = [ "kvm-amd" ];

  environment.systemPackages = with pkgs; [
    mosh wget krita gphoto2 digikam unfreeAndNonDistributablePkgs.dropbox
    firefox
  ];

  fileSystems."/boot".device = "/dev/disk/by-uuid/D54F-2AF3";
  fileSystems."/boot".fsType = "vfat";

  fileSystems."/".device = "/dev/mapper/shakti-root";
  fileSystems."/".fsType = "btrfs";
  fileSystems."/".options = [
    "compress=zstd"
    "noatime"
    "space_cache"
  ];

  swapDevices = lib.singleton {
    device = "/dev/mapper/shakti-swap";
  };

  networking.hostName = "shakti";
  networking.useNetworkd = true;

  hardware.cpu.amd.updateMicrocode = true;

  nix.maxJobs = 4;

  services.xserver.xkbOptions = "eurosign:e,caps:none";
  services.xserver.libinput.enable = true;

  services.deluge.enable = true;

  vuizvui.user.aszlig.profiles.managed.enable = true;
  vuizvui.user.aszlig.profiles.managed.mainUser = "aortab";
}
