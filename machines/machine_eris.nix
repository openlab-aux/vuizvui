{ config, pkgs, lib, ... }:

{
  imports = [ ./machine_common.nix ];

  boot = {
    loader = {
      gummiboot = {
        enable = true;
        timeout = 2;
      };

      efi.canTouchEfiVariables = true;
    };

    initrd = {
      availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" ];
      kernelModules = [ "fuse" ];
      postDeviceCommands = ''
        echo noop > /sys/block/sda/queue/scheduler
      '';
    };

    kernelModules = [ "tp_smapi" ];
    extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4788e218-db0f-4fd6-916e-e0c484906eb0";
    fsType = "btrfs";
    options = pkgs.lib.concatStringsSep "," [
      "autodefrag"
      "space_cache"
      "compress=lzo"
      "noatime"
      "ssd"
    ];
  };

  swapDevices = [ ];

  nix.maxJobs = 4;

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };
}
