{ config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  tailscaleInterface = "tailscale0";
  # tailscaleAddress = "100.76.60.85";
  # ethernetInterface = "enp0s20";

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in

{
  imports = [
    ./base-server.nix
  ];

  config = {

    # TODO abstract out
    vuizvui.modifyNixPath = false;
    nix.nixPath = [
      "nixpkgs=${with pkgs.vuizvui.profpatsch; filterSourceGitignoreWith {
          gitignoreLines =
            readGitignoreFile "${toString pkgs.path}/.gitignore";
          globMap = glob:
            # filter out the non-rooted file globs,
            # because those take forever to filter
            # (10(!) seconds evaluation time in my test).
            if (!glob.isDir && !glob.isRooted)
            then null
            else glob;
        } pkgs.path}"
      # TODO?
      # "vuizvui=/root/vuizvui"
      # TODO: nicer?
      "nixos-config=${pkgs.writeText "leguin-configuration.nix" ''
        (import <vuizvui/machines>).profpatsch.leguin.config
      ''}"
    ];

    boot.loader.grub.device = "/dev/sda";

    fileSystems = {
      "/" = {
        device = "/dev/sda3";
        fsType = "ext4";
      };
      "/boot" = {
        device = "/dev/sda2";
        fsType = "ext4";
      };
    };

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];
    };

    security.acme.acceptTerms = true;
    security.acme.email = "mail@profpatsch.de";

    networking = {
      nat = {
        # enable = true;
        # externalInterface = ethernetInterface;
        # internalInterfaces = [ wireguard.interface ];
      };

      hostName = "leguin";
      firewall = {
        allowedTCPPorts = [
          80 443
        ];
        allowedUDPPorts = [
        ];

        interfaces.${tailscaleInterface} = {
          allowedTCPPorts = [
          ];
        };
      };

      # nameservers = [
      #   "62.210.16.6"
      #   "62.210.16.7"
    };

    services.tailscale = {
      enable = true;
      # interfaceName = tailscaleInterface;
    };
  };
}
