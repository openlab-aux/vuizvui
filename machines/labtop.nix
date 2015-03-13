{ pkgs, ... }:

let
  greybird = pkgs.stdenv.mkDerivation {
    name = "greybird-xfce-theme";

    src = pkgs.fetchFromGitHub {
      repo = "Greybird";
      owner = "shimmerproject";
      rev = "61ec18d22780aa87998381599c941e0cf4f7bfb5";
      sha256 = "03h8hba4lfp337a4drylcplrbggry9gz8dq1f3gjy25fhqkgvq05";
    };

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p "$out/share/themes/Greybird" \
               "$out/share/themes/Greybird-compact/xfwm4"
      cp -vrt "$out/share/themes/Greybird" \
        gtk-* metacity-1 unity xfce-notify-4.0 xfwm4
      cp -vrt "$out/share/themes/Greybird-compact/xfwm4" \
        xfwm4_compact/*
    '';
  };

in {
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "de_DE.UTF-8";
  };

  environment.systemPackages = [
    greybird
    #repetierhost <- TODO
    pkgs.firefox
    pkgs.gimp
    pkgs.freecad
    pkgs.openscad
    pkgs.pronterface
    pkgs.blender
    pkgs.slic3r
    pkgs.libreoffice
    pkgs.inkscape
    pkgs.filezilla
    pkgs.gmpc
    pkgs.vlc
  ];

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  services.xserver.displayManager.auto.enable = true;
  services.xserver.displayManager.auto.user = "openlab";
  services.xserver.desktopManager.xfce.enable = true;

  users.mutableUsers = false;
  users.extraUsers.openlab = {
    uid = 1000;
    isNormalUser = true;
  };
}
