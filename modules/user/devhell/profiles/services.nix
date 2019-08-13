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
          enable = true;
          enableHardening = false;
        };
      };
      libvirtd = {
        enable = true;
        qemuPackage = pkgs.qemu_kvm;
      };
    };

    location.provider = "geoclue2";

    services = {
      keybase.enable = true;
      pcscd.enable = true;
      gpm.enable = true;
      openssh.enable = true;
      udisks2.enable = true;
      geoip-updater.enable = true;
      geoclue2.enable = true;
      redshift.enable = true;

      compton = {
        enable = true;
        vSync = true;
        backend = "glx";
      };
    };


    services.xserver = {
      displayManager.lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          user = "dev";
          extraConfig = ''
            [greeter]
            show-password-label = true
            password-label-text = >
            show-input-cursor = false
            [greeter-theme]
            text-color = "#4C566A"
            window-color = "#3B4252"
            border-width = 0px
            layout-space = 5
            password-background-color = "#3B4252"
          '';
        };
      };
      desktopManager.xterm.enable = false;
      desktopManager.default = "none";
    };

    services.xserver.windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
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
