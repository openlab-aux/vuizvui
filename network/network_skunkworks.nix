{ config, pkgs, lib, ... }:

{
  networking.hostName = "skunkworks";
  networking.wireless.enable = false;
  networking.useNetworkd = true;
}
