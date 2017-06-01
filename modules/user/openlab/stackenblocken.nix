{ pkgs, config, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.openlab.stackenblocken;
  package = lib.getBin pkgs.vuizvui.openlab.stackenblocken.override {
    volumePercent = cfg.volume;
  };

in
{
  options.vuizvui.user.openlab.stackenblocken = {
    enable = mkEnableOption "STACKENBLOCKEN EVERY DAY";

    volume = mkOption {
      description = "Volume in percent";
      default = 50;
      type = types.addCheck types.int (x: x >= 0 && x <= 100);
    };
  };

  config = mkIf cfg.enable {

    systemd.user = {
      services.stackenblocken = {
        description = "stackenblocken timer";
#        wantedBy = [ "default.target" ];
        serviceConfig = {
          ExecStart = "${package}/bin/stackenblocken";
        };
        # everyday at 21:45, except Wednesday (Yoga silence)
        startAt = [ "Mon,Tue,Thu,Fri,Sat,Sun 21:45" "Wed 22:00" ];
      };
    };

  };
}
