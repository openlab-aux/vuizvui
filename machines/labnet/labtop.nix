{ lib }:
let
  callMachine = import ../../lib/call-machine.nix;
  mkLabtop = hostname: config: {
    vuizvui.user.openlab.labtops.enable = true;
    networking.hostName = hostname;
  } // config;
  mkLabtops = lib.mapAttrs (name: cfg: callMachine (mkLabtop name cfg) {});

  labtop = {
    boot.loader.grub.device = "/dev/disk/by-id/ata-HITACHI_HTS722010K9SA00_080711DP0270DPGLVMPC";

    boot.kernelModules = [ "kvm-intel" ];
    boot.initrd.availableKernelModules = [
      "uhci_hcd" "ehci_pci" "ata_piix" "firewire_ohci" "usb_storage"
    ];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/754fd3e3-2e04-4028-9363-0c6bb4c54367";
      fsType = "ext4";
    };

    vuizvui.hardware.thinkpad.enable = true;

    hardware.trackpoint.enable = false;

    networking.enableIntel3945ABGFirmware = true;

  };


  labhanns = {
    nixpkgs.system = "i686-linux";
    boot.loader.grub.device = "/dev/sda";

    fileSystems."/" = {
      device = "/dev/sda";
      fsType = "ext4";
    };

  };

in
  mkLabtops { inherit labtop labhanns; }
