{ lib }:
let
  callMachine = import ../../lib/call-machine.nix;
  mkLabtop = hostname: config: {
    imports = [ config ];
    vuizvui.user.openlab.labtops.enable = true;
    networking.hostName = hostname;
  };
  mkLabtops = lib.mapAttrs (name: cfg: callMachine (mkLabtop name cfg) {});

  labtop = {

    boot.kernelModules = [ "kvm-intel" ];
    boot.initrd.availableKernelModules = [
      "uhci_hcd" "ehci_pci" "ata_piix" "firewire_ohci" "usb_storage"
    ];

    vuizvui.hardware.thinkpad.enable = true;

    hardware.trackpoint.enable = false;

    networking.enableIntel3945ABGFirmware = true;


    users.users.kevin = {
      isNormalUser = true;
      password = "kevin";
    };
    users.users.root.password = "root";

  };


  hannswurst = {
    nixpkgs.system = "i686-linux";
  };

in
  mkLabtops { inherit labtop hannswurst; }
