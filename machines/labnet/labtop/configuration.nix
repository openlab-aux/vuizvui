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
  imports = [
    ./hardware-configuration.nix
  ];

  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "de_DE.UTF-8";
  };

  fileSystems."/".label = "root";
  boot.loader.grub.device = "/dev/sda";

  environment.systemPackages = with pkgs; [
    greybird
    #repetierhost <- TODO
    firefox
    gimp
    git
    freecad
    openscad
    #pronterface <- TODO
    blender
    #slic3r
    libreoffice
    inkscape
    filezilla
    gmpc
    vlc
    vim
    wget
  ];

  # TODO: Needed for slic3r right now.
  nixpkgs.config.allowBroken = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  services.xserver.displayManager.auto.enable = true;
  services.xserver.displayManager.auto.user = "openlab";
  services.xserver.desktopManager.xfce.enable = true;

  services.openssh.enable = true;

  networking.networkmanager.enable = true;

  users.mutableUsers = false;
  users.extraUsers.openlab = {
    uid = 1000;
    isNormalUser = true;
    password = "openlab";
    extraGroups = [ "wheel" "networkmanager" ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"
    ];
  };

  # fix for emacs
  programs.bash.promptInit = "PS=\"# \"";
}
