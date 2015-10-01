{ config, pkgs, lib, ... }:

{
  virtualisation = {
    libvirtd = {
      enable = true;
      enableKVM = true;
      onShutdown = "shutdown";
    };
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
    thermald.enable = true;
    udisks2.enable = true;
  };

  services.xserver = {
    displayManager.lightdm.enable = true;
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    startGnuPGAgent = true;
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
