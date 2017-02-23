{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.openlab.base;

in
{

  options.vuizvui.user.openlab.base.enable =
    mkEnableOption "the base OpenLab configuration";


  config = mkIf cfg.enable {
    boot.loader.grub.device = mkDefault "/dev/sda";
    boot.loader.timeout = 2;

    # tmp (and nix builds) should be fast by default
    # might make sense to disable that on machines with little RAM
    boot.tmpOnTmpfs = mkDefault true;

    fileSystems."/" = mkDefault {
      device = "/dev/disk/by-label/labtop";
      fsType = "ext4";
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "us";
      defaultLocale = "de_DE.UTF-8";
    };
    time.timeZone = "Europe/Berlin";

    hardware.enableAllFirmware = true;

    # TODO: filesystems 

    environment.systemPackages = with pkgs; let
      base = [
        ack ag
        fish
        git
        netcat-openbsd
        python3
        tmux
        screen
        vim
        wget
      ];
      in base;

    # manual generation takes too long on slow machines
    programs.man.enable = mkDefault false;

    services.openssh.enable = true;

    networking.firewall.enable = false;

    users.mutableUsers = false;
    users.users.openlab = {
      uid = 1000;
      isNormalUser = true;
      password = "openlab";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = lib.singleton (lib.concatStrings [

        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLD"
        "GRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1"
        "kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZT"
        "CjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeS"
        "KZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenX"
        "H7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"
      ]);
    };
  };

}
