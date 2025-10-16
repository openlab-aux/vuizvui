# Lenovo ThinkPad T14s AMD Gen1 hardware configuration
# Based on nixos-hardware: lenovo/thinkpad/t14s/amd/gen1
{ config, lib, pkgs, ... }:

{
  # From: lenovo/thinkpad/t14s/amd/gen1
  # For mainline support of rtw89 wireless networking
  boot.kernelPackages = lib.mkIf (lib.versionOlder pkgs.linux.version "5.16") pkgs.linuxPackages_latest;

  boot.kernelParams = [
    # From: lenovo/thinkpad/t14s/amd/gen1
    # Enable energy savings during sleep
    "mem_sleep_default=deep"

    # From: lenovo/thinkpad/t14s
    # Force use of thinkpad_acpi driver for backlight control
    # This allows the backlight save/load systemd service to work
    "acpi_backlight=native"
  ];

  # From: common/cpu/amd
  # AMD CPU microcode updates
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # From: common/gpu/amd
  # AMD GPU configuration
  services.xserver.videoDrivers = lib.mkDefault [ "modesetting" ];
  hardware.graphics = {
    enable = lib.mkDefault true;
    enable32Bit = lib.mkDefault true;
  };
  hardware.amdgpu.initrd.enable = lib.mkDefault true;

  # From: lenovo/thinkpad
  # ThinkPad trackpoint configuration
  hardware.trackpoint.enable = lib.mkDefault true;
  hardware.trackpoint.emulateWheel = lib.mkDefault config.hardware.trackpoint.enable;

  # From: common/pc/laptop
  # Laptop power management
  # Gnome 40 introduced a new way of managing power, without tlp.
  # However, these 2 services clash when enabled simultaneously.
  services.tlp.enable = lib.mkDefault (
    (lib.versionOlder (lib.versions.majorMinor lib.version) "21.05")
    || !config.services.power-profiles-daemon.enable
  );

  # From: common/pc
  # Blacklist ath3k if redistributable firmware is disabled
  boot.blacklistedKernelModules = lib.optionals (!config.hardware.enableRedistributableFirmware) [
    "ath3k"
  ];
}
