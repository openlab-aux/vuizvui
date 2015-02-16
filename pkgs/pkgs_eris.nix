{ config, pkgs, lib, ... }:

{
  imports = [ ./pkgs_common.nix ];

  environment.systemPackages = with pkgs; [
    aircrackng
    kismet
    minicom
    networkmanagerapplet
    reaverwps
    snort
  ];
}
