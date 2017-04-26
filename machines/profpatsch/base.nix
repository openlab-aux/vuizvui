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

    programs.bash = {
      loginShellInit = ''
        alias c='vim /etc/nixos/configuration.nix'
        alias nsp='nix-shell -p'
        alias nrs='nixos-rebuild switch'
        alias tad='tmux attach -d'
      '';
    };

    environment.systemPackages = with pkgs; [
      curl              # transfer data to/from a URL
      file              # file information
      git               # version control system
      htop              # top replacement
      nmap              # stats about clients in the network
      rsync             # file syncing tool
      tmux              # detachable terminal multiplexer
      wget              # the other URL file fetcher
      myPkgs.vim        # slight improvement over vi
    ];

    # Nobody wants mutable state. :)
    users.mutableUsers = false;

  };

}
