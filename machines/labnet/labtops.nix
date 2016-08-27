{
  labtop = {
    vuizvui.user.openlab.labtops.enable = true;
    boot.kernelModules = [ "kvm-intel" ];
    boot.initrd.availableKernelModules = [
      "uhci_hcd" "ehci_pci" "ata_piix" "firewire_ohci" "usb_storage"
    ];

    vuizvui.hardware.thinkpad.enable = true;

    hardware.trackpoint.enable = false;

    networking.enableIntel3945ABGFirmware = true;

  };

  hannswurscht = { pkgs, ... }:
  {
    vuizvui.user.openlab.base.enable = true;

    nixpkgs.system = "i686-linux";

    users.users.openlab.extraGroups = [ "audio" ];
    services.mingetty.autologinUser = "openlab";

    hardware.pulseaudio = {
      #package = pkgs.pulseaudioLight.overrideDerivation (old: {
      #  patches = old.patches or [] ++ [ ./buffer.patch ];
      #});
      enable = true;
      tcp.enable = true;
      tcp.anonymousClients.allowedIpRanges = [ "172.16.0.0/16" ];
      zeroconf.publish.enable = true;
    };
    #services.shairport-sync.enable = true;

    vuizvui.user.openlab.stackenblocken.enable = true;

    services.logind.extraConfig = "HandleLidSwitch=ignore";
  };
}
