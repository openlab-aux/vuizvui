# this is the
# Upower daemon.
# module from nixpkgs, but with the option to suspend and less configuration

{ config, lib, pkgs, ... }:

let

  cfg = config.vuizvui.services.upower;

  ini = pkgs.formats.ini {};

  pkg = pkgs.upower.overrideAttrs (old: {
    # patches = [
    #   # Adds a "Suspend" action to what to do when the battery is critical
    #   (pkgs.fetchpatch {
    #     url = "https://gitlab.freedesktop.org/upower/upower/-/merge_requests/11.patch";
    #     sha256 = "sha256-y8ysD+fJIi5SZkWp2n061VBA5cs1EMftOof/h2tvDGo=";
    #   })
    # ];
  });

  configFile = ini.generate "UPower.conf" cfg.settings;

in

{

  ###### interface

  options = {

    vuizvui.services.upower = {

      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to enable Upower, a DBus service that provides power
          management support to applications.
        '';
      };

      settings = lib.mkOption {
        type = lib.types.nullOr ini.type;
        default = {};
        description = ''
          The upower configuration.

          So far it looks like there is always only one Section called `UPower`
        '';
        example = {
          UPower = {
            TimeCritical = 300;
            CriticalPowerAction = "Hibernate";
          };
        };
      };

    };

  };


  ###### implementation

  config = lib.mkIf cfg.enable {

    # this is … questionable … l o w  e f f o r t

    environment.systemPackages = [ pkg ];

    services.dbus.packages = [ pkg ];

    services.udev.packages = [ pkg ];

    systemd.packages = [ pkg ];

    # this implicitly assumes the package has a `upower.service` unit file
    systemd.services."upower".restartTriggers = [ configFile ];

    environment.etc."UPower/UPower.conf".source = configFile ;
  };

}
