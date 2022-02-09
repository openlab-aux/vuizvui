{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.programs.flameshot;

  # TODO: Make configurable via module system.
  settings = {
    disabledTrayIcon = true;
    drawColor = "#ff0000";
    drawThickness = 2;
    savePath = "$HOME/screenshots";
    savePathFixed = true;
    checkForUpdates = false;
  };

in {
  options.vuizvui.user.aszlig.programs.flameshot = {
    enable = lib.mkEnableOption "Flameshot";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.flameshot.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ lib.singleton ./config.patch;
        configFile = pkgs.writeText "flameshot.ini" (lib.generators.toINI {} {
          General = settings;
        });
        postPatch = (drv.postPatch or "") + ''
          substituteInPlace src/utils/confighandler.cpp --subst-var configFile
        '';
      });
      readOnly = true;
      internal = true;
      description = "The patched Flameshot package.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.singleton cfg.package;

    vuizvui.requiresTests = [
      ["vuizvui" "aszlig" "programs" "flameshot"]
    ];

    services.dbus.packages = lib.singleton (pkgs.writeTextFile {
      name = "flameshot-dbus";
      destination = "/share/dbus-1/services/org.flameshot.Flameshot.service";
      text = lib.generators.toINI {} {
        "D-BUS Service" = {
          Name = "org.flameshot.Flameshot";
          Exec = "${cfg.package}/bin/flameshot";
        };
      };
    });
  };
}
