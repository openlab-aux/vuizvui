{ config, pkgs, lib, ... }:

{
  services = {
    openssh.enable = true;
    udisks2.enable = true;
    virtualboxHost.enable = true;
    gpm.enable = true;
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

  services.journald.extraConfig = ''
    SystemMaxUse = 50M
  '';

  services.psd = {
    enable = true;
    users = [ "dev" ];
    browsers = [ "chromium" "firefox" ];
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
