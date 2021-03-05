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
    nix.useSandbox = true;

    # /tmp should never be depended on
    boot.cleanTmpDir = true;

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
      file              # file information
      htop              # top replacement
      nmap              # stats about clients in the network
      rsync             # file syncing tool
      strace            # tracing syscalls
      tmux              # detachable terminal multiplexer
      wget              # the other URL file fetcher
      myPkgs.vim        # slight improvement over vi
      lr                # list recursively, ls & find replacement
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
