{ config, lib, ... }:

with lib;

let
  which = if config.networking.useNetworkd then "networkd" else "scripted";
  networkTests = map (name: ["nixos" "networking" which name]) [
    "static" "dhcpSimple" "dhcpOneIf" "bond" "bridge" "macvlan" "sit" "vlan"
  ];

in {
  options.vuizvui = {
    requiresTests = mkOption {
      type = types.listOf (types.listOf types.str);
      default = [];
      example = [ ["nixos" "nat" "firewall"] ["vuizvui" "foo"] ];
      description = ''
        A list of attribute paths to the tests which need to succeed in order to
        trigger a channel update for the current configuration/machine.

        Every attribute path itself is a list of attribute names, which are
        queried using <function>lib.getAttrFromPath</function>.
      '';
    };
  };

  config = {
    vuizvui.requiresTests = networkTests;
  };
}
