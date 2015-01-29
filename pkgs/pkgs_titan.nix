{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    aircrackng
    atom
    kismet
    minicom
    networkmanagerapplet
    snort
  ];
}
