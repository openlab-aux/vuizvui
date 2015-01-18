{ config, pkgs, lib, ... }:

{
  networking.hostName = "eris";
  networking.networkmanager.enable = true;
}
