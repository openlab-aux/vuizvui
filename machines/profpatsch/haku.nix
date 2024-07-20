{ config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  hakuHostName = "haku.profpatsch.de";

  youtube2audiopodcastPort = 1339;
  youtube2audiopodcastSubdir = "/halp";

  sshPort = 7001;
  warpspeedPort = 1338;
  wireguardPortUdp = 6889;
  tailscaleInterface = "tailscale0";
  tailscaleAddress = "100.76.60.85";
  gonicPortTailscale = 4747;
  sambaPortTailscale = 445;

  ethernetInterface = "enp0s20";
  wireguard = {
    port = wireguardPortUdp;
    interface = "wg0";
    internalNetwork =
      let genIp = cidr: lastByte: "10.42.0.${toString lastByte}/${toString cidr}";
      in {
        addr = genIp 32;
        range = genIp 24 0;
        server = genIp 24 1;
      };
  };

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in

{
  imports = [
    ./base-server.nix
  ];

  config = {

    system.stateVersion = "22.11";

    # TODO abstract out
    vuizvui.modifyNixPath = false;
    # nix.nixPath = [
    #   "nixpkgs=${with pkgs.vuizvui.profpatsch; filterSourceGitignoreWith {
    #       gitignoreLines =
    #         readGitignoreFile "${toString pkgs.path}/.gitignore";
    #       globMap = glob:
    #         # filter out the non-rooted file globs,
    #         # because those take forever to filter
    #         # (10(!) seconds evaluation time in my test).
    #         if (!glob.isDir && !glob.isRooted)
    #         then null
    #         else glob;
    #     } pkgs.path}"
    #   # TODO?
    #   # "vuizvui=/root/vuizvui"
    #   # TODO: nicer?
    #   "nixos-config=${pkgs.writeText "haku-configuration.nix" ''
    #     (import <vuizvui/machines>).profpatsch.haku.config
    #   ''}"
    # ];

    vuizvui.user.profpatsch.server.sshPort = sshPort;

    boot.loader.grub.device = "/dev/sda";

    boot.initrd.availableKernelModules = [ "ahci" ];
    boot.kernelModules = [ "kvm-intel" ];

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

    swapDevices = [
      { device = "/dev/sda4"; }
    ];

    hardware.cpu.intel.updateMicrocode = true;

    environment.systemPackages = with pkgs; [
      mktorrent                         # torrent file creator
      # pkgs.vuizvui.profpatsch.warpspeed # trivial http file server
    ];

    # users.groups.data-seeding = {};

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];

      # rtorrent = {
      #   isNormalUser = true;
      #   extraGroups = [ "data-seeding" ];
      # };

      # youtube2audiopodcast = {
      #   isSystemUser = true;
      #   group = "youtube2audiopodcast";
      # };
    };

    # semi-tmp
    # vuizvui.services.drawpile = {
    #   enable = true;
    #   configFile = ''
    #     [config]
    #     serverTitle = bsalc
    #     sessionSizeLimit = 200MB
    #     sessionCountLimit = 1
    #     persistence = true
    #     idleTimeLimit = 0
    #     title = Welcome to the bsalc server!
    #   '';
    # };

    vuizvui.services.profpatsch.gonic = {
      enable = true;
      listenAddress = "${tailscaleAddress}:${toString gonicPortTailscale}";
      musicDir = "/var/lib/transmission/Downloads";
      musicDirGroup = "transmission";
      podcastDir = "/var/lib/gonic/podcasts";
      podcastDirGroup = "transmission";
      playlistsDir = "/var/lib/gonic/playlists";
      playlistsDirGroup = "transmission";
      scanIntervalMinutes = 10;
    };
    systemd.services.gonic.serviceConfig.wantedBy = [ "tailscaled.target" ];

    # services.samba = {
    #   enable = true;
    #   enableNmbd = false;
    #   enableWinbindd = false;
    #   nsswins = false;
    #   extraConfig = ''
    #     # only listen to tailscale
    #     interfaces = ${tailscaleInterface}
    #     smb ports = ${toString sambaPortTailscale}
    #   '';
    #   shares = {
    #     data-seeding = {
    #       "path" = "/data/seeding";
    #       "read only" = "yes";
    #       "browsable" = "yes";
    #       "guest ok" = "yes";
    #     };
    #   };
    # };
    # # somewhat hacky, but we want tailscale to be up
    # systemd.services.samba-smbd.wants = [ "tailscaled.service" ];
    # systemd.services.samba-smbd.after = [ "tailscaled.service" ];

    # systemd.services.warpspeed =
    #   let user = config.users.users.rtorrent;
    #   in {
    #     description = "internally served public files (see nginx)";
    #     wantedBy = [ "default.target" ];
    #     serviceConfig.WorkingDirectory = "${user.home}/public";
    #     # *6: all hosts, v6 preferred
    #     script = ''${pkgs.vuizvui.profpatsch.warpspeed}/bin/warpspeed "*6" ${toString warpspeedPort}'';
    #     serviceConfig.User = config.users.users.rtorrent.name;
    #   };

    # systemd.services.youtube2audiopodcast =
    #   let user = config.users.users.youtube2audiopodcast;
    #   in {
    #     description = "serve a youtube playlist as rss";
    #     wantedBy = [ "default.target" ];
    #     script = "${pkgs.vuizvui.profpatsch.youtube2audiopodcast {
    #       url = "https://${hakuHostName}${youtube2audiopodcastSubdir}";
    #       internalPort = toString youtube2audiopodcastPort;
    #     }}";
    #     serviceConfig.User = config.users.users.youtube2audiopodcast.name;
    #   };


    security.acme.acceptTerms = true;
    security.acme.defaults.email = "mail@profpatsch.de";

    # services.nginx = {
    #   enable = true;
    #   virtualHosts.${hakuHostName} = {
    #     forceSSL = true;
    #     enableACME = true;
    #     locations."/pub/" = {
    #       proxyPass = "http://127.0.0.1:${toString warpspeedPort}/";
    #     };
    #     locations."${youtube2audiopodcastSubdir}/" = {
    #       proxyPass = "http://127.0.0.1:${toString youtube2audiopodcastPort}/";
    #     };
    #     locations."/".root =
    #       let lojbanistanSrc = pkgs.fetchFromGitHub {
    #         owner = "lojbanistan";
    #         repo = "lojbanistan.de";
    #         rev = "ef02aa8f074d0d5209839cd12ba7a67685fdaa05";
    #         sha256 = "1hr2si73lam463pcf25napfbk0zb30kgv3ncc0ahv6wndjpsvg7z";
    #       };
    #       in pkgs.runCommandLocal "lojbanistan-www" {} ''
    #         mkdir $out
    #         echo "coi do" > $out/index.html
    #         ${pkgs.imagemagick}/bin/convert \
    #           ${lojbanistanSrc}/design/flag-of-lojbanistan-icon.svg \
    #           -define icon:auto-resize=64,48,32,16 \
    #           $out/favicon.ico
    #       '';
    #     serverAliases = [ "lojbanistan.de" ];
    #   };
    # };

    networking = {
      hostName = "haku";

      useNetworkd = true;

      interfaces.enp0s20.useDHCP = true;

      firewall = {
        allowedTCPPorts = [
          80 443
          6882
          1337 2342 4223
          60100
        ];
        allowedUDPPorts = [
          60100
        ];

        # warning: Strict reverse path filtering breaks Tailscale exit node use and some subnet routing setups. Consider setting `networking.firewall.checkReversePath` = 'loose'
        checkReversePath = "loose";

        interfaces.${tailscaleInterface} = {
          allowedTCPPorts = [
            gonicPortTailscale
            sambaPortTailscale
          ];
        };
      };

      nameservers = [
        "62.210.16.6"
        "62.210.16.7"
      ];
    };

    services.tailscale = {
      enable = true;
      # interfaceName = tailscaleInterface;
    };
  };
}
