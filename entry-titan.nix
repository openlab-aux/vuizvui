{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./machines/machine_titan.nix
      ./network/network_titan.nix
      ./services/services_titan.nix
      ./pkgs/pkgs_titan.nix
      ./overrides/overrides.nix
      ./users/users_dev.nix
    ];
}

