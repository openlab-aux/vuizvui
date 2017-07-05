{ config, lib, pkgs, ... }:

with lib;

let
  bin = drv: name: "${lib.getBin drv}/bin/${name}";
  cfg = config.vuizvui.user.openlab.speedtest;

  py = pkgs.runCommand "speedtest.py" {} ''
    cat ${./speedtest.py} \
      | sed -e 's|^PING_BIN =.*$|PING_BIN = "${config.security.wrapperDir}/ping"|' \
      > $out
  '';

  speedtest = pkgs.writeScript "speedtest" ''
    #!${bin pkgs.bash "bash"}
    ${bin pkgs.python3 "python3"} ${py} >> $HOME/data.yaml
  '';

in {
  options.vuizvui.user.openlab.speedtest.enable =
    mkEnableOption "openlab speedtest";

  config = mkIf cfg.enable {
    systemd.services.speedtest = {
       description = "openlab network speedtest";
       path = with pkgs; [ curl bind.host ];
       environment = { "LC_ALL" = "C"; };
       wantedBy = [ "default.target" ];
       after = [ "network.target" ];
       script = "${speedtest}";
       startAt = [ "*-*-* *:00/15:00" ];
       serviceConfig.User = "speedtest";
     };

     users.users.speedtest = {
        createHome = true;
        home = "/var/lib/speedtest";
     };
  };
}
