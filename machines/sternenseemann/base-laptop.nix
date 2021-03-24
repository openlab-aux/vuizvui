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

    hardware.bluetooth.enable = lib.mkDefault true;
    services.blueman.enable = true;

    services.pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      socketActivation = true;
    };

    hardware.cpu.intel.updateMicrocode = true;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    vuizvui.hardware.thinkpad.enable = lib.mkDefault true;
    vuizvui.hardware.low-battery = {
      enable = true;
      treshold = 3;
      action = "hibernate";
    };

    environment.systemPackages = with pkgs; [
      lr
      lowdown
      lynx sacc
      zip unzip
      stow
      silver-searcher
      mosh
      nmap
      ffmpeg graphicsmagick
      pavucontrol
    ] ++ pkgs.vuizvui.sternenseemann.scripts.default;

    services.earlyoom = {
      enable = true;
      freeMemThreshold = 5;
    };
  };
}
