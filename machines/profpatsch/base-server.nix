{ config, pkgs, ... }:

let
  sshPort = 6879;

in
{
  imports = [
    ./base.nix
  ];

  config = {

    boot.cleanTmpDir = true;

    programs.mosh.enable = true;

    services.openssh = {
      enable = true;
      listenAddresses = [ { addr = "0.0.0.0"; port = sshPort; } ];
    };

    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ sshPort ];
    };


  };
