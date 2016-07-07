{ config, pkgs, lib, ... }:

{
  imports = [ ./network_common.nix ];

  networking.hostName = "eris";
  networking.wireless.enable = true;
  networking.connman.enable = true;
  networking.wicd.enable = false;
  networking.networkmanager.enable = false;
}
