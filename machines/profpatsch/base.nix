# A base configuration that still assumes a workstation
{ pkgs, lib, ... }:
let
  myPkgs = import ./pkgs.nix { inherit pkgs lib; };
  fish = pkgs.fish;

  authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];

  philip = rec {
    name = "philip";
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    createHome = true;
    home = "/home/philip";
    passwordFile = "${home}/.config/passwd";
    shell = "${lib.getBin fish}/bin/fish";
    openssh.authorizedKeys.keys = authKeys;
  };


in {

  config = {

    boot.loader = {
      grub.enable = true;
      grub.version = 2;
    };

    networking = {
      wireless.enable = false;
      firewall.enable = false;
      networkmanager.enable = true;
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "neo";
      defaultLocale = "en_US.UTF-8";
    };

    programs.ssh.startAgent = false;


    ###########
    # Packages

    environment.sessionVariables = { EDITOR = "${myPkgs.vim}/bin/vim"; };

    environment.systemPackages = with pkgs;
    let
      # of utmost necessity for me to function
      basePkgs = [
        silver-searcher   # file content searcher, > ack > grep
        curl              # transfer data to/from a URL
        dos2unix          # text file conversion
        file              # file information
        git               # version control system
        htop              # top replacement
        manpages          # system manpages (not included by default)
        mkpasswd          # UNIX password creator
        nmap              # stats about clients in the network
        rsync             # file syncing tool
        smartmontools     # check disk state
        stow              # dotfile management
        tmux              # detachable terminal multiplexer
        traceroute        # trace ip routes
        wget              # the other URL file fetcher
        wirelesstools     # iwlist (wifi scan)
        myPkgs.vim        # slight improvement over vi
      ];
    in basePkgs;

    # friendly user shell
    programs.fish.enable = true;

    ###########
    # Services

    services.openssh.enable = true;

    time.timeZone = "Europe/Berlin";

    # bounded journal size
    services.journald.extraConfig = "SystemMaxUse=50M";


    ########
    # Users

    # Nobody wants mutable state. :)
    users.mutableUsers = false;
    users.users = { inherit philip; };

  };
}
