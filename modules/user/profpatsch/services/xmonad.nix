{pkgs, lib, config, ...}:

let
  cfg = config.vuizvui.user.profpatsch.xserver.windowManager.xmonad;


in {
  options = {
    vuizvui.user.profpatsch.xserver.windowManager.xmonad = {
      enable = lib.mkEnableOption "xmonad";

      package = lib.mkOption {
        default = pkgs.haskellPackages.xmonad;
        type = lib.types.package;
        description = ''
          Xmonad binary/package to use.
        '';
      };

      xmonadCliArgs = lib.mkOption {
        default = [];
        type = with lib.types; listOf str;
        description = ''
          Command line arguments passed to the xmonad binary.
        '';
      };

      updateSessionEnvironment = lib.mkOption {
        default = true;
        type = lib.types.bool;
        description = ''
          Whether to run dbus-update-activation-environment and systemctl import-environment before session start.
          Required for xdg portals to function properly.
        '';
      };

    };
  };
  config = lib.mkIf cfg.enable {
    services.xserver.windowManager = {
      session = [{
        name = "xmonad";
        start = ''
           ${lib.optionalString cfg.updateSessionEnvironment ''
              ${pkgs.systemd}/bin/systemctl --user import-environment PATH DISPLAY XAUTHORITY DESKTOP_SESSION XDG_CONFIG_DIRS XDG_DATA_DIRS XDG_RUNTIME_DIR XDG_SESSION_ID DBUS_SESSION_BUS_ADDRESS
              ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd --all
           ''}
           systemd-cat -t xmonad -- ${cfg.package}/bin/xmonad ${lib.escapeShellArgs cfg.xmonadCliArgs} &
           waitPID=$!
        '';
      }];
    };

    environment.systemPackages = [ cfg.package ];
  };
}
