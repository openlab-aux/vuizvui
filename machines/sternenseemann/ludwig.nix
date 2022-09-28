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

    nixpkgs = {
      localSystem = {
        system = "i686-linux";
        config = "i686-unknown-linux-gnu";
      };

      overlays = [
        (self: super: {
          tlp = super.tlp.overrideAttrs (_: {
            # Avoid crash in pandoc which is a check dependency of tlp
            # https://gitlab.haskell.org/ghc/ghc/-/issues/22002
            doCheck = false;
          });

          upower = super.upower.overrideAttrs (_: {
            # https://gitlab.freedesktop.org/upower/upower/-/issues/214
            doCheck = false;
          });
        })
      ];
    };

    system.stateVersion = "22.05";

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
        "sd_mod"
        "sr_mod"
        "usblp"
        "pcspkr"
        "btusb"
      ];

      initrd.luks.devices = {
        crypted-main.device = "/dev/disk/by-uuid/84dc044f-3137-435f-a7ee-67112e56cfaa";
        crypted-swap.device = "/dev/disk/by-uuid/f84ab2a9-2687-4674-a870-d037bbd84640";
      };
    };

    fileSystems = {
      "/" = {
        device = "/dev/mapper/crypted-main";
        fsType = "ext4";
      };

      "/boot" = {
        device = "/dev/disk/by-label/boot";
        fsType = "ext4";
      };
    };

    swapDevices = [
      { device = "/dev/mapper/crypted-swap"; }
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
