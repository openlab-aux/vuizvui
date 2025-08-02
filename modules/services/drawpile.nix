{ pkgs, lib, config, ... }:

let
  cfg = config.vuizvui.services.drawpile;

  port = 27750;
  adminPort = 9876;
  stateDir = "/var/lib/drawpile";

in {

  options = {

    vuizvui.services.drawpile = {
      enable = lib.mkEnableOption "drawpile dedicated server";

      configFile = lib.mkOption {
        type = lib.types.str;
        description = ''
          The ini configuration file of the server.
          See <https://drawpile.net/help/server/>.
        '';
      };

    };
  };


  config = lib.mkIf cfg.enable {
    systemd.services.drawpile = {
      description   = "drawpile headless server";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];

      serviceConfig = {
        Restart = "always";
        RestartSec = "1s";
        KillSignal = "SIGINT";
        DynamicUser = true;
        StateDirectory = "drawpile";
        UMask = "0007";
        ExecStart = toString [
          "${pkgs.drawpile-server-headless}/bin/drawpile-srv"
          "--config" (pkgs.writeText "drawpile-server.ini" cfg.configFile)
          # implicit from StateDirectory
          "--sessions" "/var/lib/drawpile"
          "--port" (toString port)
          "--web-admin-port" (toString adminPort)
        ];

        # Sandboxing
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectControlGroups = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
        RestrictRealtime = true;
        RestrictNamespaces = true;
        MemoryDenyWriteExecute = true;
      };
    };

    networking.firewall.allowedTCPPorts = [ port ];

  };

}
