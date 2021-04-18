# starts bitlbee and creates a socket in /run/bitlbee.socket
# which accepts one client.
{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.profpatsch.services.bitlbee;
  bitlbeeUsername = "bitlebee";
  stateDir = "bitlbee";

  # based on the example config https://github.com/bitlbee/bitlbee/blob/master/bitlbee.conf
  bitlbeeConfig = pkgs.writeText "bitlbee.conf" (lib.generators.toINI {} {
    settings = {
      RunMode = "Inetd";
      User = bitlbeeUsername;
      AuthMode = "Open";
      AuthBackend = "storage";
      ConfigDir = "/var/lib/" + stateDir;
      Protocols = "jabber";
      # is this okay?
      CAFile = "/etc/ssl/certs/ca-certificates.crt";
    };
  });

in

{

  ###### interface

  options = {

    vuizvui.user.profpatsch.services.bitlbee = {

      enable = lib.mkEnableOption "bitlbee";

      socketFile = lib.mkOption {
        description = ''
          Where to put the unix socket.
          It will be accessible by users in the `bitlbee` group.
        '';
        type = lib.types.str;
        default = "/run/bitlbee.socket";
      };
    };
  };

  ###### implementation

  config = (lib.mkIf cfg.enable {
    users.users.bitlbee = {
      description = "BitlBee user";
      home = "/var/lib/bitlbee";
      createHome = true;
      isNormalUser = true;
    };

    users.groups.bitlbee.name = "bitlbee";

    systemd.services."bitlbee@" = {
      description = "BitlBee";
      after = [ "network.target" ];
      serviceConfig = {
        User = "bitlbee";
        ExecStart =
        "${pkgs.bitlbee}/bin/bitlbee -v -c ${bitlbeeConfig}";
        StateDirectory = "bitlbee";
        # To get the inetd input
        StandardInput = "socket";
      };
    };

    # bitlbee
    systemd.sockets.bitlbee = {
      description = "bitlbee socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        # Run in inetd mode
        Accept = true;
        # Only one client at a time
        MaxConnections = 1;
        ListenStream = cfg.socketFile;
        SocketUser = "bitlbee";
        SocketGroup = "bitlbee";
        SocketMode = "0660";
      };
    };

  });

}
