# Base config shared by all machines
{ pkgs, config, lib, ... }:

let
  # TODO: inject into every config from outside
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

in
{
  config = {
    # correctness before speed
    nix.settings.sandbox = true;

    nix.settings.max-jobs = "auto";

    # /tmp should never be depended on
    boot.cleanTmpDir = true;

    # Setting it to UTC explicitely makes it impossible to override at runtime
    time.timeZone = lib.mkDefault "UTC";

    # Set default input keymapping to neo (haha sorry everybody)
    console = {
      font = "lat9w-16";
      keyMap = "neo";
    };

    # the kernel OOM is not good enough without swap,
    # and I don’t like swap. This kills the most hoggy
    # processes when the system goes under a free space limit
    services.earlyoom = {
      enable = true;
      freeMemThreshold = 5; # <5% free
    };

    # bounded journal size
    services.journald.extraConfig = "SystemMaxUse=500M";

    programs.bash = {
      interactiveShellInit = ''
        alias c='vim /root/vuizvui/machines/profpatsch'
        alias nsp='nix-shell -p'
        alias nrs='nixos-rebuild switch'
        alias tad='tmux attach -d'
        alias gs='git status'

        # search recursively in cwd for file glob (insensitive)
        findia () { find -iname "*''${*}*"; }
        # like findia, but first argument is directory
        findian () { path="$1"; shift; find $path -iname "*''${*}*"; }
        # like findian, but searches whole filepath
        findiap () { path="$1"; shift; find $path -ipame "*''${*}*"; }
      '';
    };

    environment.systemPackages = with pkgs; [
      curl              # transfer data to/from a URL
      binutils          # debugging binary files
      dos2unix          # text file conversion
      file              # file information
      htop              # top replacement
      ncdu              # disk size checker
      nmap              # stats about clients in the network
      man-pages          # system manpages (not included by default)
      mkpasswd          # UNIX password creator
      lr                # list recursively, ls & find replacement
      ripgrep           # file content searcher, > ag > ack > grep
      rsync             # file syncing tool
      strace            # tracing syscalls
      tmux              # detachable terminal multiplexer
      traceroute        # trace ip routes
      wget              # the other URL file fetcher
      myPkgs.vim        # slight improvement over vi
      xe                # xargs with a modern interface
    ];

    i18n = {
      defaultLocale = "en_US.UTF-8";
      extraLocaleSettings = {
        LC_TIME = "de_DE.UTF-8";
      };
    };

    # Nobody wants mutable state. :)
    users.mutableUsers = false;

  };

}
