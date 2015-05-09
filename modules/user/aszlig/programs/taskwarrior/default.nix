{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.programs.taskwarrior;

  taskrc = pkgs.writeText "taskrc.in" ''
    data.location=~/.task
    include @out@/share/doc/task/rc/dark-yellow-green.theme

    color=on
    dateformat=Y-m-d
    dateformat.annotation=Y-m-d
    dateformat.edit=Y-m-d H:N:S
    dateformat.holiday=YMD
    dateformat.info=Y-m-d H:N:S
    dateformat.report=Y-m-d
    weekstart=Monday
  '';

  taskwarrior = pkgs.taskwarrior.overrideDerivation (t: {
    patches = (t.patches or []) ++ [ ./config.patch ];
    postInstall = (t.postInstall or "") + ''
      mkdir -p "$out/etc"
      substituteAll "${taskrc}" "$out/etc/taskrc"
    '';
  });

in {
  options.vuizvui.user.aszlig.programs.taskwarrior = {
    enable = lib.mkEnableOption "aszlig's TaskWarrior";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = lib.singleton taskwarrior;
  };
}
