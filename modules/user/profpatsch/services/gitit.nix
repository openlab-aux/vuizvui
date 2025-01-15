{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.user.profpatsch.services.gitit;

  gititPkg = pkgs.vuizvui.profpatsch.gitit;

in {

  options.vuizvui.user.profpatsch.services.gitit = {
    enable = lib.mkEnableOption "gitit wiki";

    config = lib.mkOption {
      type = lib.types.anything;
      default = {};
      description = "Gitit configuration file, as json structure; will overwrite the default configuration.";
    };

    stateDirName = lib.mkOption {
      type = lib.types.str;
      default = "gitit";
      description = "Name of the directory in /var/lib where the gitit state is stored. This is private to our service, and always mounted to the same location. The Configuration file is evaluated relative to that directory.";
    };
  };


  config = lib.mkIf cfg.enable {
    systemd.services.gitit =
      let path = [ pkgs.coreutils pkgs.gnugrep pkgs.git ];
      in {
        description = "Gitit wiki server";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          inherit path;
          confinement = {
            enable = true;
            packages = path;
          };
          serviceConfig = {
            Type = "simple";
            ExecStart = pkgs.vuizvui.profpatsch.writeExecline "gitit-ExecStart" {} [
              "cd" "/var/lib/${cfg.stateDirName}"
              "${gititPkg}/bin/gitit"
              "-f" (pkgs.writers.writeJSON "gitit-config.json" cfg.config)
            ];
            User = "nobody";
            Group = "nogroup";
            Restart = "always";
            StateDirectory = cfg.stateDirName;
          };
      };
  };
}
