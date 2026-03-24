{ modulesPath, config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  hostname = "legosi";

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";
  qwerkyKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM3ORvMbdHaJfgPgMhWTqgVrP1L7kkvuETQpzl0IjP2G tc@windoof";

  tailscaleInterface = "tailscale0";
  tailscaleAddress = "100.89.52.54";

  xandikosPort = 2345;
in {
  imports = [
    ./base-server.nix
    "${modulesPath}/profiles/qemu-guest.nix"
    "${import ../../pkgs/profpatsch/home-repo-src.nix}/users/Profpatsch/website"
    "${import ../../pkgs/profpatsch/home-repo-src.nix}/users/Profpatsch/softwaregardening"
    "${import ../../pkgs/profpatsch/home-repo-src.nix}/users/Profpatsch/decentsoftware"
  ];

  config = {
    profpatsch.website.enable = true;
    profpatsch.softwaregardening.enable = true;
    profpatsch.decentsoftware.enable = true;
    profpatsch.softwaregardening.boosterBotAdmins = [
      "https://mastodon.xyz/users/Profpatsch"
    ];
    _module.args.depot = pkgs.vuizvui.profpatsch.homeRepo;

    vuizvui.modifyNixPath = false;
    nix = {
      nixPath = [
        # cop out, if you really need a tool locally on the server, just use latest unstable
        "nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
      ];

      settings.auto-optimise-store = true;
      settings.min-free = 3 * 1024 * 1024 * 1024;
    };

    system.stateVersion = "23.11";

    vuizvui.user.profpatsch.server.sshPort = 7001;

    boot.loader.grub.device = "/dev/sda";

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

        interfaces.${tailscaleInterface} = {
          allowedTCPPorts = [
            xandikosPort
          ];
        };
      };

    };

    services.tailscale = {
      enable = true;
      interfaceName = tailscaleInterface;
    };

    # services.duplicity = {
    #   enable = true;
    #   frequency = "daily";

    #   root = "/";
    #   # exclude all the system-related dirs
    #   exclude = [
    #     "/bin"
    #     "/boot"
    #     "/dev"
    #     "/nix"
    #     "/proc"
    #     "/run"
    #     "/sys"
    #     "/tmp"
    #     "/usr"
    #     # /var/lib is what we want because it contains all services,
    #     # but let’s be generous and keep everything in /var except log
    #     "/var/log"
    #   ];

    #   targetUrl = "b2://000efe88f7148a00000000003@profpatsch-legosi/";

    #   # this uses the internal stateDirectory of the duplicity module
    #   # Has to be set manually once of course.
    #   secretFile = "/var/lib/duplicity/secrets";

    #   extraFlags = [
    #     "--name" "legosi-root"
    #     "--verbosity" "info"
    #     "--full-if-older-than" "60D"
    #     "--num-retries" "3"
    #     # I hate GPG from the bottom of my heart
    #     "--encrypt-key" gpgPublicKeyId
    #     "--gpg-options" "--keyring ${gpgPublicKeyring} --trust-model always"
    #   ];

    # };

    environment.systemPackages = [ pkgs.git ];

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];
    };

    vuizvui.user.profpatsch.programs.weechat = [
      {
        userName = "weechat";
        # give this user access to the bitlbee group and socket
        extraGroups = [ "bitlbee" ];
        weechatDataDir = "/var/lib/weechat";
        authorizedKeys = [ myKey ];
        # redirect the bitlbee unix socket to a fake domain
        # because weechat is unable to connect to unix sockets.
        wrapExecStart = [
          "${pkgs.ip2unix}/bin/ip2unix"
          "-r"
          "addr=1.2.3.4,port=6667,path=${config.vuizvui.user.profpatsch.services.bitlbee.socketFile}"
        ];
      }
    ];

    vuizvui.user.profpatsch.services.bitlbee = {
       enable = true;
    };

    security.acme.acceptTerms = true;
    security.acme.defaults.email = "mail@profpatsch.de";

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
    };

    services.xandikos = {
      enable = true;
      # not exposed via the firewall, should only be accessible via tailscale
      address = tailscaleAddress;
      port = xandikosPort;
      extraOptions = [
        "--autocreate"
        "--defaults"
        "--dump-dav-xml"
      ];
    };

    # services.syncthing = {
    #   enable = true;
    #   openDefaultPorts = true;
    #   devices = {
    #     mushu = {
    #       name = "mushu";
    #       id = "B5H23ZS-NANQF55-2TW7V4F-22I7B34-7OIZ3ND-PHWL6JP-IIEVBBK-32RKWQR";
    #     };
    #   };

    #   folders.mushu-data = {
    #     id = "so6nv-oq5wu";
    #     path = "/sync/mushu-data";
    #     type = "receiveonly";
    #     watch = false;
    #     devices = [ "mushu" ];
    #   };
    # };
  };
}
