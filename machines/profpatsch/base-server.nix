{ config, pkgs, lib, ... }:

let
  cfgImports = (import ../../pkgs/profpatsch/nixos-toml-modules.nix { inherit lib; }).readAnyToml ./base-server.toml
    config;

in
{
  inherit (cfgImports) imports;

  # TODO: cannot read options from pkgs because it would lead to an infinite recursion
  # in the module system, since the pkgs passed into this module already requires all options.
  options = ((import ../../pkgs/profpatsch/nixos-toml-modules.nix { inherit lib; }).readAnyToml ./base-server-options.toml).options
    ;

  config =
    cfgImports.config
    # TODO: how to handle a reference to pkgs?
    // {
      # This can’t be in base.nix, because the workstations
      # have gitFull which leads to env collisions.
      environment.systemPackages = [ pkgs.git ];
      };


  # options.vuizvui.user.profpatsch.server.sshPort = lib.traceValSeqN 3 (lib.mkOption {
  #   description = "ssh port";
  #   # TODO: replace with types.intBetween https://github.com/NixOS/nixpkgs/pull/27239
  #   type = with lib.types; addCheck int (x: x >= 0 && x <= 65535);
  #   default = 6879;
  # });

  # config = {

  #   programs.mosh.enable = true;

  #   services.openssh = {
  #     enable = true;
  #     listenAddresses = [ { addr = "0.0.0.0"; port = cfg.sshPort; } ];
  #   };

  #   networking.firewall = {
  #     enable = true;
  #     allowPing = true;
  #     allowedTCPPorts = [ cfg.sshPort ];
  #   };

  # };

}
