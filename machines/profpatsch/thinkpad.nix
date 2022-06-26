# hardware-specific configuration for Profpatsch’s thinkpads.
{ lib, config, pkgs, ... }:

{
  imports = [];

  config = {
    # Enables drivers, acpi, power management
    vuizvui.hardware.thinkpad.enable = true;
  };
}
