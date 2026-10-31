{ pkgs, config, lib, utils, ... }:

let
  cfg = config.vuizvui.user.aszlig.services.niri;
  inherit (lib) types;

in {
  options.vuizvui.user.aszlig.services.niri = {
    enable = lib.mkEnableOption "Niri";

    outputs = lib.mkOption {
      type = types.attrsOf types.lines;
      default = {};
      example.DP1 = ''
        scale 2
        transform "normal"
      '';
      description = "Custom per-output configuration in KDL format.";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.niri.enable = true;
    programs.niri.package = pkgs.vuizvui.aszlig.niri.override {
      inherit (cfg) outputs;
    };
    programs.niri.useNautilus = false;

    systemd.user.services.kitty = {
      description = "Kitty Terminal";

      requires = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      enableDefaultPath = false;

      serviceConfig.Type = "oneshot";
      serviceConfig.ExecStart = toString [
        (utils.escapeSystemdExecArg "${config.systemd.package}/bin/systemctl")
        "--user" "start" "kitty@\${INVOCATION_ID}.service"
      ];
    };

    systemd.user.services."kitty@" = {
      description = "Kitty Terminal Instance";

      requires = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      enableDefaultPath = false;

      serviceConfig.ExecStart = utils.escapeSystemdExecArgs [
        (lib.getExe pkgs.vuizvui.aszlig.kitty) "--single-instance"
      ];
    };

    systemd.user.services.anyrun-daemon = {
      description = "Anyrun Program Launcher Daemon";

      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      enableDefaultPath = false;

      serviceConfig.BusName = "org.anyrun.anyrun";
      serviceConfig.ExecStart = utils.escapeSystemdExecArgs [
        (lib.getExe pkgs.vuizvui.aszlig.anyrun) "daemon"
      ];
    };

    systemd.user.services.anyrun = {
      description = "Anyrun Program Launcher";

      requires = [ "graphical-session.target" "anyrun-daemon.service" ];
      after = [ "graphical-session.target" "anyrun-daemon.service" ];
      enableDefaultPath = false;

      serviceConfig.Type = "oneshot";
      serviceConfig.ExecStart = lib.getExe pkgs.vuizvui.aszlig.anyrun;
    };

    systemd.user.services.mako = {
      description = "Lightweight Wayland notification daemon";
      documentation = [ "man:mako(1)" ];

      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];

      serviceConfig.BusName = "org.freedesktop.Notifications";
      serviceConfig.ExecStart = lib.getExe pkgs.vuizvui.aszlig.mako;
      serviceConfig.ExecReload = utils.escapeSystemdExecArgs [
        (lib.getExe pkgs.vuizvui.aszlig.mako) "reload"
      ];
    };

    systemd.user.services.ironbar = {
      description = "Ironbar Status Bar";

      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];

      serviceConfig.ExecStart = lib.getExe pkgs.vuizvui.aszlig.ironbar;
      serviceConfig.KillSignal = "SIGINT";
    };

    services.systemd-lock-handler.enable = true;

    systemd.user.services.swaylock = {
      description = "Lock Session";

      onSuccess = [ "unlock.target" ];
      wantedBy = [ "lock.target" ];
      partOf = [ "lock.target" ];
      requires = [ "graphical-session.target" ];
      before = [ "lock.target" ];
      after = [ "graphical-session.target" ];

      serviceConfig.Type = "oneshot";
      serviceConfig.Restart = "on-failure";
      serviceConfig.RestartSec = 0;
      serviceConfig.ExecStart = utils.escapeSystemdExecArgs [
        (lib.getExe pkgs.swaylock) "--color" "000000" "--show-failed-attempts"
      ];
    };
  };
}
