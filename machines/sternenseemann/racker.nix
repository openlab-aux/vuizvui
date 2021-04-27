{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./base-laptop.nix
    ./fonts.nix
    ./user-lukas.nix
  ];

  config = {
    hardware.bluetooth.enable = false;

    nixpkgs.localSystem = {
      system = "i686-linux";
      config = "i686-unknown-linux-gnu";
    };

    system.stateVersion = "21.05";

    boot = {
      loader.grub = {
        enable = true;
        version = 2;
        device = "/dev/sda";
      };

      initrd.availableKernelModules = [
        "uhci_hcd"
        "ehci_pci"
        "ata_piix"
        "usb_storage"
        "floppy"
        "usblp"
        "pcspkr"
        "btusb"
      ];

      initrd.luks.devices = {
        main.device = "/dev/disk/by-label/crypted-main";
        swap.device = "/dev/disk/by-label/crypted-swap";
      };
    };

    fileSystems = {
      "/" = {
        device = "/dev/mapper/main";
        fsType = "btrfs";
      };

      "/boot" = {
        device = "/dev/disk/by-label/boot";
        fsType = "ext4";
      };
    };

    swapDevices = [
      { device = "/dev/mapper/swap"; }
    ];

    hardware.enableRedistributableFirmware = true;

    networking.hostName = "racker";

    nix.maxJobs = 2;

    vuizvui.profiles.tvl.enable = true;
  };
}
