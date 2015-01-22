{ config, pkgs, lib, ... }:

{
  nixpkgs.config.conky.wireless = true;

  environment.systemPackages = with pkgs; [
    aircrackng
    atom
    kismet
    minicom
    networkmanagerapplet
    snort
  ];
}
