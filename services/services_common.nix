{ config, pkgs, lib, ... }:

{
  services = {
    openssh.enable = true;
    openssh.permitRootLogin = "false";
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
        type "pulse"
        name "Pulse Output"
      }
      replaygain "album"
    '';
  };
}
