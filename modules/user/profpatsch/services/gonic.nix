{ config, lib, pkgs, ... }:

let
  cfg = config.vuizvui.services.profpatsch.gonic;
  gonicDataDir = "/var/lib/gonic";
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

     musicDir = lib.mkOption {
       description = "path to the music directory";
       type = lib.types.path;
     };
     musicDirGroup = lib.mkOption {
       description = "user group to access music directory";
       type = lib.types.str;
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
         ExecStart = writeExecline "start-gonic" {} [
           bins.gonic
           "-listen-addr" "127.0.0.1:4747"
           "-cache-path" "${gonicDataDir}/cache"
           "-db-path" "${gonicDataDir}/db.sqlite"
           "-music-path" cfg.musicDir
         ];
         StateDirectory = gonicDataDir;
         User = userName;
         SupplementaryGroups = cfg.musicDirGroup;
       };
     };

   };
}
