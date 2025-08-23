{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./user-lukas.nix
  ];

  config = {
    hardware.bluetooth.enable = false;

    nixpkgs = {
      localSystem = {
        system = "i686-linux";
        config = "i686-unknown-linux-gnu";
      };

      overlays = [
        (self: super: {
          upower = super.upower.overrideAttrs (_: {
            # https://gitlab.freedesktop.org/upower/upower/-/issues/214
            doCheck = false;
          });
        })
      ];
    };

    system.stateVersion = "25.05";

    boot = {
      loader.grub = {
        enable = true;
        device = "/dev/sda";
      };

      initrd.availableKernelModules = [
        "uhci_hcd"
        "ehci_pci"
        "ata_piix"
        "usb_storage"
        "floppy"
        "sd_mod"
        "sr_mod"
        "usblp"
        "pcspkr"
        "btusb"
      ];

      initrd.luks.devices = {
        "crypt-main".device = "/dev/disk/by-uuid/d21e8b57-74bc-4884-b7ac-570e14b6456a";
        "crypt-swap".device = "/dev/disk/by-uuid/9d067c39-26e0-4d2f-a99f-f2d55e75a85d";
      };
    };

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-uuid/2f7ec1c7-734d-446e-afdb-0b490efa591e";
        fsType = "btrfs";
      };

      "/boot" = {
        device = "/dev/disk/by-uuid/61bfe5c8-4d72-4324-b64a-dfe3d0f7c357";
        fsType = "ext2";
      };
    };

    swapDevices = [
      { device = "/dev/disk/by-uuid/1ea845d7-e491-47b0-b3ac-62f4d789056f"; }
    ];

    hardware.enableRedistributableFirmware = true;

    networking = {
      hostName = "ludwig";
      enableIntel2200BGFirmware = true;
    };

    nix.settings = {
      max-jobs = 2;
      cores = 2;
    };

    vuizvui.profiles.tvl.enable = true;
  };
}
