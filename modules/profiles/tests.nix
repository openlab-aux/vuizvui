{ config, lib, ... }:

with lib;

let
  which = if config.networking.useNetworkd then "networkd" else "scripted";
  networkTests = map (name: ["nixos" "networking" which]) [
    "static" "dhcpSimple" "dhcpOneIf" "bond" "bridge" "macvlan" "sit" "vlan"
  ];

in {
  config = {
    vuizvui.requiresTests = networkTests;
  };
}
