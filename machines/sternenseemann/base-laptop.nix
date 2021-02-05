# TODO(sterni): split out common stuff for personal
#               computers when I have a non-laptop one
{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./fonts.nix
  ];

  config = {
    console.keyMap = "neo";

    hardware.enableRedistributableFirmware = lib.mkDefault true;

    hardware.bluetooth.enable = lib.mkDefault true;
    services.blueman.enable = true;

    hardware.pulseaudio = {
      daemon.config.flat-volumes = "yes";
      enable = true;
    };

    hardware.cpu.intel.updateMicrocode = true;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    services.tlp.enable = lib.mkDefault true;

    hardware.trackpoint = lib.mkDefault {
      enable = true;
      emulateWheel = true;
    };

    environment.systemPackages = with pkgs; [
      tree
      lowdown
      lynx sacc
      acpi
      zip unzip
      stow
      neovim silver-searcher
      mosh
      nmap
      ffmpeg graphicsmagick
      kitty pavucontrol
      xsel
    ] ++ pkgs.vuizvui.sternenseemann.scripts.default;
  };
}
