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

    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = with pkgs; [
        rofi-wayland
        alacritty
        waybar
        i3status-rust
        swayidle
        swaylock-effects
        wl-clipboard
        swaybg
      ];
    };

    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
          user = "greeter";
          vt = 3;
        };
      };
    };

    services.dbus.packages = [ pkgs.mako ];

    services = {
      pcscd.enable = true;
      gpm.enable = true;
      openssh.enable = true;
      udisks2.enable = true;
      haveged.enable = true;
      redshift = {
        enable = true;
        package = pkgs.gammastep;
      };
      geoclue2 = {
        enable = true;
        enable3G = false;
        enableCDMA = false;
        enableDemoAgent = true;
        enableModemGPS = false;
        enableNmea = false;
        enableWifi = true;
        submitData = true;
      };

      globalprotect = {
        enable = true;
        csdWrapper = "${pkgs.openconnect}/libexec/openconnect/hipreport.sh";
      };

      picom = {
        enable = false;
        vSync = true;
        backend = "glx";
        fade = true;
        fadeDelta = 1;
        shadow = true;
        shadowExclude = [
          "window_type = 'menu'"
          "window_type = 'utility'"
          "window_type = 'dropdown_menu'"
          "window_type = 'popup_menu'"
        ];
        settings = {
          inactive-dim = 0.2;
          unredir-if-possible = false;
        };
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
