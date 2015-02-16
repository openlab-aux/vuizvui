{ config, pkgs, lib, ... }:

{
  imports = [ ./network_common.nix ];

  networking.hostName = "titan";
  networking.wireless.enable = false;
  networking.connman.enable = false;
  networking.wicd.enable = false;
  networking.networkmanager.enable = true;
}
