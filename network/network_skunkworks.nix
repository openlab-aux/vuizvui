{ config, pkgs, lib, ... }:

{
  imports = [ ./network_common.nix ];

  networking.hostName = "skunkworks";
  networking.wireless.enable = false;
  networking.useNetworkd = false;
}
