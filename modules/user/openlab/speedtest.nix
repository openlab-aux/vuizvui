{ config, lib, pkgs, ... }:

with lib;

let
  bin = drv: name: "${lib.getBin drv}/bin/${name}";
  cfg = config.vuizvui.user.openlab.speedtest;

  py = pkgs.runCommandLocal "speedtest.py" {} ''
    cat ${./speedtest.py} \
      | sed -e 's|^PING_BIN =.*$|PING_BIN = "${config.security.wrapperDir}/ping"|' \
      > $out
  '';

  speedtest = pkgs.writeScript "speedtest" ''
    #!${bin pkgs.bash "bash"}
    mkdir -p "$(dirname "${cfg.outputPath}")"
    ${bin pkgs.python3 "python3"} ${py} >> "${cfg.outputPath}"
  '';

in {
  options.vuizvui.user.openlab.speedtest = {
    enable = mkEnableOption "openlab speedtest";
    outputPath = mkOption {
      description = "File to which the results are appended.";
      type = types.path;
      default = "/dev/null";
    };
  };


  config = mkIf cfg.enable {
    systemd.services.speedtest = {
       description = "openlab network speedtest";
       path = with pkgs; [ curl bind.host ];
       environment = { "LC_ALL" = "C"; };
       wantedBy = [ "default.target" ];
       after = [ "network.target" ];
       script = "${speedtest}";
       startAt = [ "*-*-* *:00/15:00" ];
     };

     assertions = [ {
       assertion = cfg.outputPath != "/dev/null";
       message = "You should set `vuizvui.user.openlab.speedtest.outputPath`.";
     } ];
  };
}
