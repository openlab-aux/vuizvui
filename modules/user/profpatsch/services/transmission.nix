{pkgs, lib, config, ... }:

let
  cfg = cfg.vuizvui.user.profpatsch.programs.transmission;

{
  config = {
    users = {
      groups.${cfg.userGroup} = {};
      users = {
        users.${cfg.userName} = {
          isSystemUser = true;
          createHome = true;
          shell = bins.dash;
          group = cfg.userGroup;
          home = cfg.transmissionDataDir;
          extraGroups = cfg.extraGroups;
        };
      };
    };
  };
}
