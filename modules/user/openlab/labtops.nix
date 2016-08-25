{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.openlab.labtops;

in
{

  options.vuizvui.user.openlab.labtops = {
    enable = mkEnableOption "basic shared functionality of labtops";
  };


  config = mkIf cfg.enable {

    vuizvui.user.openlab.base.enable = true;

    # TODO: a way to modularly specify usage patterns (e.g. 3d-printing, arduino &c.)
    environment.systemPackages = with pkgs; let
      baseGUI = [
        filezilla
        chromium
        gnome3.gedit
        gmpc
        libreoffice
        vlc
      ];
      image = [
        gimp
        inkscape
      ];
      media = [
        mpv
        vlc
        pavucontrol
      ];
      three-d = [
        # TODO doesn’t build on i686
        # TODO add a “packageset” mechanism
        blender
        # TODO build fail
        # antimony
      ];
      three-d-printing = [
        freecad
        openscad
        printrun
        slic3r
      ];
      arduinoPkgs = [
        ino
        arduino
      ];
      tools = [
        unzip
      ];
      in baseGUI ++ image ++ media
      ++ three-d ++ three-d-printing ++ arduinoPkgs
      ++ tools;

    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";

      displayManager.auto.enable = true;
      displayManager.auto.user = "openlab";
      desktopManager.xfce.enable = true;
      synaptics = {
        enable = true;
        minSpeed = "0.5";
        accelFactor = "0.01";
      };
    };

    users.users.openlab.extraGroups = [ "dialout" ];

    # fix for emacs
    programs.bash.promptInit = "PS=\"# \"";

    programs.man.enable = true;

  };

}
