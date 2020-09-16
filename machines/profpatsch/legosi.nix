{ modulesPath, config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  hostname = "legosi";

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in {
  imports = [
    ./base-server.nix
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  config = {
    vuizvui.modifyNixPath = false;
    nix = {
      nixPath = [
        "vuizvui=/root/vuizvui"
        "nixpkgs=/root/nixpkgs"
        # todo: nicer?
        "nixos-config=${pkgs.writeText "legosi-configuration.nix" ''
          (import <vuizvui/machines>).profpatsch.legosi.config
        ''}"
      ];

      extraOptions = ''
        auto-optimise-store = true
        min-free = ${toString (3 * 1024 * 1024 * 1024)}
      '';

    };

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
      hostName = hostname;

      firewall = {
        allowedTCPPorts = [
          80 443
        ];
      };

      interfaces.ens3 = {
        ipv6.addresses = [{
          address = "2a01:4f8:c0c:70aa::1";
          prefixLength = 64;
        }];
      };

      defaultGateway6 = {
        address = "fe80::1";
        interface = "ens3";
      };
    };


    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];
    };

    vuizvui.programs.profpatsch.weechat = {
      enable = true;
      authorizedKeys = [ myKey ];
      # redirect the bitlbee unix socket to a fake domain
      # because weechat is unable to connect to unix sockets.
      wrapExecStart = [
        "${pkgs.ip2unix}/bin/ip2unix"
        "-r"
        "addr=1.2.3.4,port=6667,path=${config.vuizvui.services.profpatsch.bitlbee.socketFile}"
      ];
    };
    users.users.weechat.extraGroups = [ "bitlbee" ];

    vuizvui.services.profpatsch.bitlbee = {
       enable = true;
    };

    security.acme.acceptTerms = true;
    security.acme.email = "mail@profpatsch.de";

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      virtualHosts.${"profpatsch.de"} = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          index = "index.html";
          root = pkgs.vuizvui.profpatsch.websiteStatic;
        };
      };
    };

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      declarative = {
        devices = {
          mushu = {
            name = "mushu";
            id = "B5H23ZS-NANQF55-2TW7V4F-22I7B34-7OIZ3ND-PHWL6JP-IIEVBBK-32RKWQR";
          };
        };

        folders.mushu-data = {
          id = "so6nv-oq5wu";
          path = "/sync/mushu-data";
          type = "receiveonly";
          watch = false;
          devices = [ "mushu" ];
        };
      };
    };

  };
}
