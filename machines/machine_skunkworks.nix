{ config, pkgs, lib, ... }:

{
  imports = [ ./machine_common.nix ];

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      devices = [
        "/dev/disk/by-id/ata-ST31500541AS_6XW0NK21"
        "/dev/disk/by-id/ata-ST31500541AS_6XW0P0CW"
      ];
    };

    initrd = {
      availableKernelModules = [ "ehci_pci" "ahci" "firewire_ohci" "usbhid" "usb_storage" ];
      kernelModules = [ "fuse" ];
    };

    kernelParams = [ "pci=noaer" ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    blacklistedKernelModules = [ "pcspkr" ];
  };

  fileSystems."/" = {
    label = "nixos";
    fsType = "btrfs";
    options = pkgs.lib.concatStringsSep "," [
      "autodefrag"
      "space_cache"
      "compress=lzo"
      "noatime"
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/16bd9abd-6af5-4a24-8ea5-58adc51e9641"; }
    { device = "/dev/disk/by-uuid/279708cb-f9c3-4a37-a064-80ff85a66f88"; }
  ];

  nix.maxJobs = 8;

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "dvorak";
    defaultLocale = "en_US.UTF-8";
  };

  powerManagement.powerUpCommands = ''
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0NK21
    ${pkgs.hdparm}/sbin/hdparm -B 255 \
  /dev/disk/by-id/ata-ST31500541AS_6XW0P0CW
  '';
}
