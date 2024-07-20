{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.profpatsch.server;

in
{
  imports = [ ./base.nix ];

  options = {
    vuizvui.user.profpatsch.server.sshPort = lib.mkOption {
      description = "ssh port";
      default = 7001;
      type = lib.types.port;
    };
  };


  config = {
    programs.mosh.enable = true;

    services.openssh = {
      enable = true;
      listenAddresses = [{
        addr = "0.0.0.0";
        port = cfg.sshPort;
      }];
    };

    networking.enableIPv6 = false;

    networking.firewall = {
      enable = true;
      allowPing = true;

      allowedTCPPorts = [ cfg.sshPort ];
    };

  };
}
