{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.profpatsch.services.cas-serve;

in

{

  ###### interface

  options = {

    vuizvui.user.profpatsch.services.cas-serve = {
      enable = lib.mkEnableOption "cas-serve";
    };
  };

  ###### implementation

  config = (lib.mkIf cfg.enable {
    users.users.cas-serve = {
      description = "cas-serve user";
      home = "/var/lib/cas-serve";
      createHome = true;
      isNormalUser = false;
    };

    users.groups.cas-serve.name = "cas-serve";

    systemd.services.cas-serve = {
      description = "cas-serve";
      after = [ "network.target" ];
      serviceConfig = {
        User = "cas-serve";
        ExecStart =
        "${pkgs.vuizvui.profpatsch.tvl.users.Profpatsch.cas-serve}/bin/cas-serve";
        StateDirectory = "cas-serve";
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
