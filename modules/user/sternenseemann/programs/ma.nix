{ pkgs, config, lib, ... }:

let
  cfg = config.vuizvui.user.sternenseemann.programs.ma;
in

{
  options = {
     vuizvui.user.sternenseemann.programs.ma = {
       enable = lib.mkEnableOption "ma editor";
       package = lib.mkOption {
         type = lib.types.package;
         description = "ma derivation to use";
         default = pkgs.ma;
         defaultText = lib.literalExpression "pkgs.ma";
       };
     };
   };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # TODO(sterni): set environment in the service
    # TODO(sterni): start automatically
    systemd.user.services.ma-registry = rec {
      description = "ma editor's file registry";
      after = [ "graphical-session.target" ];
      partOf = after;
      serviceConfig = {
        Type = "simple";
        ExecStart = "${lib.getExe' cfg.package "ma"} -registry";
      };
    };
  };
}
