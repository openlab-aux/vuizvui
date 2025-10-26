{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.devhell.profiles.services;

in {
  options.vuizvui.user.devhell.profiles.services = {
    enable = lib.mkEnableOption "Services profile for devhell";
  };

  config = lib.mkIf cfg.enable {
    virtualisation = {
      virtualbox = {
        host = {
          enable = false;
          enableHardening = false;
        };
      };
      libvirtd = {
        enable = true;
        qemu.package = pkgs.qemu_kvm;
      };
      podman.enable = true;
    };

    location.provider = "geoclue2";

    programs.niri.enable = true;

    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.tuigreet}/bin/tuigreet --time --cmd niri-session";
          user = "greeter";
          vt = 2;
        };
      };
    };

    services.dbus.packages = [ pkgs.mako ];

    services = {
      avahi.enable = true;
      pcscd.enable = true;
      gpm.enable = true;
      openssh.enable = true;
      udisks2.enable = true;
      haveged.enable = true;
      automatic-timezoned.enable = true;
      geoclue2 = {
        enable = true;
        enableWifi = true;
        enableNmea = true;
        enableModemGPS = false;
        enableCDMA = false;
        enable3G = false;
      };
    };

    services.pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
      wireplumber.enable = true;
      socketActivation = true;
    };

    services.journald.extraConfig = ''
      SystemMaxUse = 50M
    '';
  };
}
