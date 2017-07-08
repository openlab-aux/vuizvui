{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.profpatsch.server;

in
{
  imports = [
    ./base.nix
  ];

  options.vuizvui.user.profpatsch.server.sshPort = lib.mkOption {
    description = "ssh port";
    # TODO: replace with types.intBetween https://github.com/NixOS/nixpkgs/pull/27239
    type = with lib.types; addCheck int (x: x >= 0 && x <= 65535);
    default = 6879;
  };

  config = {

    boot.cleanTmpDir = true;

    programs.mosh.enable = true;

    services.openssh = {
      enable = true;
      listenAddresses = [ { addr = "0.0.0.0"; port = cfg.sshPort; } ];
    };

    networking.firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ cfg.sshPort ];
    };

  };

}
