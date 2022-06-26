# hardware-specific configuration for Profpatsch’s thinkpads.
{ lib, config, pkgs, ... }:

{
  imports = [];

  config = {
    boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
    boot.loader.systemd-boot.enable = true;
    boot.loader.grub.enable = false;

    # Enables drivers, acpi, power management
    vuizvui.hardware.thinkpad.enable = true;

    vuizvui.services.upower.enable = lib.mkDefault true;
  };
}
