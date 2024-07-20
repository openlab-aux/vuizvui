{pkgs, lib, config, ...}:

let
  cfg = config.vuizvui.user.profpatsch.xserver.windowManager.xmonad;

in {
  options = {
    vuizvui.user.profpatsch.xserver.windowManager.xmonad = {
      enable = lib.mkEnableOption (lib.mdDoc "xmonad");

      package = lib.mkOption {
        default = pkgs.xmonad;
        type = lib.types.package;
        description = ''
          Xmonad binary/package to use.
        '';
      };

      xmonadCliArgs = lib.mkOption {
        default = [];
        type = with lib.types; listOf str;
        description = lib.mdDoc ''
          Command line arguments passed to the xmonad binary.
        '';
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.xserver.windowManager = {
      session = [{
        name = "xmonad";
        start = ''
           systemd-cat -t xmonad -- ${cfg.package}/bin/xmonad ${lib.escapeShellArgs cfg.xmonadCliArgs} &
           waitPID=$!
        '';
      }];
    };

    environment.systemPackages = [ cfg.package ];
  };
}
