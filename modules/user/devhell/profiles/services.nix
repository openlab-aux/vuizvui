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
        qemuPackage = pkgs.qemu_kvm;
      };
      podman.enable = true;
    };

    location.provider = "geoclue2";

    services = {
      pcscd.enable = true;
      gpm.enable = true;
      openssh.enable = true;
      udisks2.enable = true;
      geoip-updater.enable = true;
      geoclue2.enable = true;
      redshift.enable = true;

      picom = {
        enable = true;
        vSync = true;
        backend = "glx";
        experimentalBackends = true;
        fade = true;
        fadeDelta = 1;
        shadow = true;
        settings = { 
          inactive-dim = 0.2;
          unredir-if-possible = false;
        };
      };
    };


    services.xserver = {
      displayManager.defaultSession = "none+i3";
      displayManager.lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          user = "dev";
          extraConfig = ''
            [greeter]
            show-password-label = true
            password-label-text = ❯
            show-input-cursor = false
            [greeter-theme]
            border-color = "#3B4252"
            text-color = "#4C566A"
            window-color = "#3B4252"
            layout-space = 5
            password-background-color = "#3B4252"
            border-width = 0px
            password-border-width = 0px
          '';
        };
      };
    };

    services.xserver.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [ polybar i3status-rust i3blocks-gaps rofi ]
        ++ lib.optional (pkgs ? i3-auto-layout) [ pkgs.i3-auto-layout ];
      # FIXME: lib.optional construct can be removed if i3-auto-layout is in unstable
    };

    services.journald.extraConfig = ''
      SystemMaxUse = 50M
    '';

    services.mpd = {
      enable = true;
      extraConfig = ''
        input {
          plugin "curl"
        }

        audio_output {
          type "fifo"
          name "FIFO Output"
          path "/tmp/mpd.fifo"
          format "44100:16:2"
        }

        audio_output {
          type "pulse"
          name "Pulse Output"
          server "127.0.0.1"
        }

        replaygain "album"
      '';
    };
  };
}
