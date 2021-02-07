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

    hardware.pulseaudio = {
      daemon.config.flat-volumes = "yes";
      enable = true;
    };

    hardware.cpu.intel.updateMicrocode = true;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

    vuizvui.hardware.thinkpad.enable = lib.mkDefault true;

    environment.systemPackages = with pkgs; [
      tree
      lowdown
      lynx sacc
      zip unzip
      stow
      silver-searcher
      mosh
      nmap
      ffmpeg graphicsmagick
      kitty pavucontrol
      xsel
    ] ++ pkgs.vuizvui.sternenseemann.scripts.default;
  };
}
