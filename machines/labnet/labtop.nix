{ lib }:
let
  compose = f: g: x: f (g x);
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

    networking.wireless = {
      interfaces = [ "wlp2s0" ];
    };
    environment.etc."wpa_supplicant.conf".text = ''
      network={
      	ssid="Labor 2.0"
      	#psk="nerdhoehle2"
      	psk=5d925de5243d3a77a86e803ff6ac1f02ce7e1606f23bdb3d8ca60d90f26e8684
      }
    '';
  };

in
  mkLabtops { inherit labtop labhanns; }
