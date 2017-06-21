{ config, pkgs, lib, ... }:

{
  imports = [ ./network_common.nix ];

  networking.hostName = "eris";
  networking.networkmanager.enable = true;
}
