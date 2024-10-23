{ config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  tailscaleInterface = "tailscale0";

  hostname = "leguin";

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in

{
  imports = [
    ./base-server.nix
    (import ./profpatsch-thinkpad.nix { cpuType = "intel"; })
  ];

  config = {

    # TODO abstract out
    vuizvui.modifyNixPath = false;
    nix = {
      nixPath = [
        # cop out, if you really need a tool locally on the server, just use latest unstable
        "nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
      ];

      settings.auto-optimise-store = true;
      settings.min-free = 3 * 1024 * 1024 * 1024;
    };

    boot.initrd.luks.devices.cryptroot.device = "/dev/disk/by-label/root";

    fileSystems."/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "ssd" ];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
      options = ["nofail"];
    };

    vuizvui.user.profpatsch.server.sshPort = 7001;

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];
    };

    security.acme.acceptTerms = true;
    security.acme.defaults.email = "mail@profpatsch.de";

    networking = {

      hostName = hostname;
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

    };

    services.logind.extraConfig = lib.generators.toKeyValue {} {

      # I don’t want the machine to go off immediately
      # when I accidentally touch the power button.
      HandlePowerKey = "ignore";
      HandlePowerKeyLongPress = "poweroff";

      # as long as the machine is connected to power,
      # it needs to be closed to preserve the monitor
      HandleLidSwitchExternalPower = "ignore";
      HandleLidSwitchDocked = "ignore";

    };

    services.tailscale = {
      enable = true;
      interfaceName = tailscaleInterface;
    };
    # strict filtering breaks some tailscale features
    networking.firewall.checkReversePath = "loose";
  };
}
