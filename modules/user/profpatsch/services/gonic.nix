{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.services.profpatsch.gonic;
  gonicDataDir = "/var/lib/gonic";
  systemdDataDirSuffix = "gonic"; # implicitly scoped to /var/lib
  userName = "gonic";

  inherit (pkgs.vuizvui.profpatsch)
    writeExecline
    lru-dir
    getBins
    ;

  bins = getBins pkgs.gonic [ "gonic" ]
    ;

in {

   options.vuizvui.services.profpatsch.gonic = {
     enable = lib.mkEnableOption "gonic server";

     listenAddress = lib.mkOption {
       description = "daemon listen address";
       type = lib.types.str;
     };

     musicDir = lib.mkOption {
       description = "path to the music directory; must exist beforehand and have the right group permissions";
       type = lib.types.path;
     };
     musicDirGroup = lib.mkOption {
       description = "user group to access music directory";
       type = lib.types.str;
     };

     podcastDir = lib.mkOption {
       description = "path to the podcast directory; must exist beforehand and have the right group permissions";
       type = lib.types.path;
     };
     podcastDirGroup = lib.mkOption {
       description = "user group to access podcast directory";
       type = lib.types.str;
     };

     scanIntervalMinutes = lib.mkOption {
       description = "interval (in minutes) to check for new music (automatic scanning disabled if omitted)";
       type = lib.types.nullOr lib.types.ints.positive;
       default = null;
       example = 10;
     };
   };

   config = lib.mkIf cfg.enable {
     users.users.${userName} = {
       isSystemUser = true;
       createHome = true;
       home = gonicDataDir;
     };

     systemd.services.gonic = {
       wantedBy = [ "default.target" ];
       serviceConfig = {
         ExecStart = writeExecline "start-gonic" {} ([
           bins.gonic
           "-listen-addr" cfg.listenAddress
           "-cache-path" "${gonicDataDir}/cache"
           "-db-path" "${gonicDataDir}/db.sqlite"
           "-music-path" cfg.musicDir
           "-podcast-path" cfg.podcastDir
         ]
         ++ lib.optionals (cfg.scanIntervalMinutes != null) [
           "-scan-interval" (toString cfg.scanIntervalMinutes)
         ]);
         StateDirectory = systemdDataDirSuffix;
         User = userName;
         SupplementaryGroups = [ cfg.musicDirGroup cfg.podcastDirGroup ];
       };
     };

   };
}
