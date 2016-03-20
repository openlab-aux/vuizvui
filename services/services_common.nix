{ config, pkgs, lib, ... }:

{
  virtualisation = {
    virtualbox = {
      host = {
        enable = true;
        enableHardening = true;
      };
    };
  };

  services = {
    gpm.enable = true;
    openssh.enable = true;
    haveged.enable = true;
    thermald.enable = true;
    udisks2.enable = true;
    redshift = {
      enable = true;
      latitude = "51.2750";
      longitude = "1.0870";
    };
  };

  services.xserver = {
    displayManager.lightdm.enable = true;
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
  };

  services.xserver.windowManager = {
    i3.enable = true;
    default = "i3";
  };

  services.syncthing = {
    enable = true;
    user = "dev";
  };

  services.journald.extraConfig = ''
    SystemMaxUse = 50M
  '';

  services.psd = {
    enable = true;
    users = [ "dev" ];
    browsers = [ "chromium" ];
  };

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
}
