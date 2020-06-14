{ modulesPath, config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in {
  imports = [
    ./base-server.nix
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  config = {
    nix.nixPath = [
      "vuizvui=/root/vuizvui"
      "nixpkgs=/root/nixpkgs"
      # todo: nicer?
      "nixos-config=${pkgs.writetext "legosi-configuration.nix" ''
        (import <vuizvui/machines>).profpatsch.legosi.config
      ''}"
    ];

    vuizvui.user.profpatsch.server.sshPort = 7001;

    boot.loader.grub.device = "/dev/sda";
    # VPN support
    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];

    fileSystems = {
      "/" = {
        device = "/dev/sda1";
        fsType = "ext4";
      };
    };

    networking = {
      hostName = "legosi";
    };

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];
    };

    environment.systemPackages = [
      pkgs.weechat
    ];
  };
}
