{ config, pkgs, lib, ... }:

{
  nixpkgs.config.conky.wireless = true;

  environment.systemPackages = with pkgs; [
    aircrackng
    kismet
    minicom
    networkmanagerapplet
    snort
  ];
}
