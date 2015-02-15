{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    aircrackng
    kismet
    minicom
    networkmanagerapplet
    reaverwps
    snort
  ];
}
