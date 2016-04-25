{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.openlab.labtops;

in
{

  options.vuizvui.user.openlab.labtops = {
    enable = mkEnableOption "basic shared functionality of labtops";

  };



  config = mkIf cfg.enable {
    boot.loader.grub.device = mkDefault "/dev/sda";
    fileSystems."/" = mkDefault {
      device = "/dev/disk/by-label/labtop";
      fsType = "ext4";
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "us";
      defaultLocale = "de_DE.UTF-8";
    };

    # TODO: filesystems 

    # TODO: a way to modularly specify usage patterns (e.g. 3d-printing, arduino &c.)
    environment.systemPackages = with pkgs; let
      base = [
        ack
        fish
        git
        netcat-openbsd
        python3
        tmux
        screen
        vim
        wget
      ];
      baseGUI = [
        filezilla
        chromium
        gnome3.gedit
        gmpc
        libreoffice
        vlc
      ];
      image = [
        gimp
        inkscape
      ];
      three-d = [
        # TODO doesn’t build on i686
        # TODO add a “packageset” mechanism
        # blender
        # TODO build fail
        # antimony
      ];
      three-d-printing = [
        freecad
        openscad
        printrun
        slic3r
      ];
      arduino = [
        ino
      ];
      in base ++ baseGUI ++ image ++ three-d ++ three-d-printing ++ arduino;

    services.xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e";

      displayManager.auto.enable = true;
      displayManager.auto.user = "openlab";
      desktopManager.xfce.enable = true;
      synaptics = {
        enable = true;
        minSpeed = "0.5";
        accelFactor = "0.01";
      };
    };

    services.openssh.enable = true;

    networking.firewall.enable = false;
    networking.wireless = {
      enable = true;
      networks."Labor 2.0".psk = "nerdhoehle2";
    };

    # TODO: an argument that tells about hardware capability
    nix.maxJobs = 2;
    hardware.enableAllFirmware = true;

    users.mutableUsers = false;
    users.users.openlab = {
      uid = 1000;
      isNormalUser = true;
      password = "openlab";
      extraGroups = [ "wheel" "networkmanager" "dialout"];
      openssh.authorizedKeys.keys = lib.singleton (lib.concatStrings [

        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLD"
        "GRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1"
        "kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZT"
        "CjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeS"
        "KZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenX"
        "H7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"
      ]);
    };

    # fix for emacs
    programs.bash.promptInit = "PS=\"# \"";

  };

}
