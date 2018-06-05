{ pkgs, ... }:

let
  musicDir = "/data/music";
  webserverRootDir = "/var/www";

in {
  vuizvui.user.openlab.base.enable = true;

  nixpkgs.system = "i686-linux";

  users.users.openlab.extraGroups = [ "audio" ];
  services.mingetty.autologinUser = "openlab";

  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
    package = pkgs.pulseaudioFull;
    zeroconf.discovery.enable = false;
    zeroconf.publish.enable = true;
    tcp.enable = true;
    tcp.anonymousClients.allowedIpRanges = [ "172.16.0.0/16" "127.0.0.1" ];
  };

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };

  hardware.sane.enable = true;
  services.saned = {
    enable = true;
    extraConfig = ''
      172.16.0.0/16
    '';
  };

  # TODO: haskell module broken
  # vuizvui.user.openlab.stackenblocken = {
  #   enable = true;
  #   volume = 35;
  # };
  vuizvui.user.openlab.speedtest = {
    enable = true;
    outputPath = "${webserverRootDir}/speedtest.yaml";
  };

  services.nginx = {
    enable = true;
    virtualHosts."hannswurscht.openlab.lan" = {
      default = true;
      root = webserverRootDir;
    };
  };

  # machine mostly runs headless with the screen shut
  services.logind.extraConfig = "HandleLidSwitch=ignore";

  fileSystems = {
    "${musicDir}" = {
      device = "ftp.openlab.lan:/data/upload/music";
      fsType = "nfs";
      label = "lab-ftp";
      options = [ "nolock" "x-systemd.automount"];
    };
  };
}
