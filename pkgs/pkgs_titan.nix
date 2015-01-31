{ config, pkgs, lib, ... }:

{
  nixpkgs.config.mpv = {
    vaapiSupport = true;
  };

  environment.systemPackages = with pkgs; [
    aircrackng
    atom
    kismet
    minicom
    networkmanagerapplet
    snort
  ];
}
