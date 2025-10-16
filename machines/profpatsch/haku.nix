{ config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };
  homeRepo = pkgs.vuizvui.profpatsch.homeRepo;

  hakuHostName = "haku.profpatsch.de";
  testHostName = "test.profpatsch.de";
  matrixHostName = "matrix.decentsoftwa.re";

  youtube2audiopodcastPort = 1339;
  youtube2audiopodcastSubdir = "/halp";

  sshPort = 7001;
  httzipPort = 7070;
  openlabToolsPort = 9099;
  wireguardPortUdp = 6889;
  tailscaleInterface = "tailscale0";
  tailscaleAddress = "100.122.12.129";
  gonicPortTailscale = 4747;
  transmissionDownloadDirectory = "/var/lib/transmission/Downloads";
  transmissionDownloadDirectoryGroup = "transmission";
  whatcdResolverReverseProxyPortTailscale = 9092;
  whatcdResolverPortTailscale = 9093;
  whatcdResolverJaegerPortTailscale = 16686;
  sambaPortTailscale = 445;
  dentritePort = 8008;

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

    # make sure /boot does not run out of space
    boot.loader.systemd-boot.configurationLimit = 5;

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
    ];

    users.groups.whatcd-resolver = {};
    users.groups.openlab-tools = {};

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];

      zipped-transmission = {
        isSystemUser = true;
        group = transmissionDownloadDirectoryGroup;
      };
      whatcd-resolver = {
        isSystemUser = true;
        home = "/var/lib/whatcd-resolver";
        createHome = true;
        group = "whatcd-resolver";
        extraGroups = [ transmissionDownloadDirectoryGroup ];
      };
      openlab-tools = {
        isSystemUser = true;
        home = "/var/lib/openlab-tools";
        createHome = true;
        group = "openlab-tools";
      };

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
      musicDir = transmissionDownloadDirectory;
      musicDirGroup = transmissionDownloadDirectoryGroup;
      podcastDir = "/var/lib/gonic/podcasts";
      podcastDirGroup = transmissionDownloadDirectoryGroup;
      playlistsDir = "/var/lib/gonic/playlists";
      playlistsDirGroup = transmissionDownloadDirectoryGroup;
      scanIntervalMinutes = 10;
    };
    systemd.services.gonic.serviceConfig.wantedBy = [ "tailscaled.target" ];

    # TODO: this is horrible lol
    systemd.services.httzip =
      let user = config.users.users.zipped-transmission;
      in {
        description = "internally served public files (see nginx)";
        wantedBy = [ "default.target" ];
        serviceConfig.WorkingDirectory = transmissionDownloadDirectory;
        script = ''${homeRepo.users.Profpatsch.httzip}'';
        serviceConfig.User = user.name;
      };


    # TODO: this is horrible lol
    systemd.services.whatcd-resolver =
      let user = config.users.users.whatcd-resolver;
      in {
        description = "what?";
        wantedBy = [ "default.target" ];
        serviceConfig.WorkingDirectory = "/var/lib/whatcd-resolver";
        script = "${pkgs.vuizvui.profpatsch.writeExecline "run-whatcd-resolver-jaeger" {} [
          "envfile" "/var/lib/whatcd-resolver/whatcd-resolver-env"
          homeRepo.users.Profpatsch.whatcd-resolver.whatcd-resolver
        ]}";
        serviceConfig.User = user.name;
        # transmission extra group
        serviceConfig.Environment = "WHATCD_RESOLVER_TRANSMISSION_DOWNLOAD_DIRECTORY=${transmissionDownloadDirectory}";
      };
    systemd.services.whatcd-resolver-jaeger =
      let user = config.users.users.whatcd-resolver;
      in {
        description = "what? jaeger";
        wantedBy = [ "default.target" "whatcd-resolver.service" ];
        serviceConfig.WorkingDirectory = "/var/lib/whatcd-resolver/jaeger";
        # webui: 16686, otel: 4318
        script = ''${pkgs.vuizvui.profpatsch.jaeger}/bin/jaeger-all-in-one'';
        serviceConfig.User = user.name;
      };

    # TODO: this is horrible lol
    systemd.services.openlab-tools =
      let user = config.users.users.openlab-tools;
      in {
        description = "tooling for openlabs";
        wantedBy = [ "default.target" ];
        serviceConfig.WorkingDirectory = "/var/lib/openlab-tools";
        script = ''${homeRepo.users.Profpatsch.openlab-tools}'';
        serviceConfig.User = user.name;
      };

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

    services.nginx = {
      enable = true;
      virtualHosts.${hakuHostName} = {
        forceSSL = true;
        enableACME = true;
        locations."/public/" = {
          root = "/var/www";
          index = "index.html";
          extraConfig = ''
            sendfile on;
            sendfile_max_chunk 1m;
          '';
        };
        locations."/zipped/" = {
          proxyPass = "http://127.0.0.1:${toString httzipPort}/";
        };
        locations."/openlab-tools/" = {
          proxyPass = "http://127.0.0.1:${toString openlabToolsPort}/";
        };

        locations."/opengraph-experiment/".root = pkgs.linkFarm "opengraph-player-root" [
            { name = "opengraph-experiment/index.html";
            path = pkgs.writeText "index.html" ''
                <!DOCTYPE html>
                <html>

                <head>
                    <title>Opengraph player example</title>
                    <meta property="twitter:card" content="player">

                    <meta property="og:title" content="Opengraph rich embed example">
                    <meta property="og:type" content="music.song">
                            <meta property="og:site_name" content="NONELOL">
                    <meta property="og:description" content="You would not believe your eyes">
                    <meta property="og:video"
                      content="https://haku.profpatsch.de/opengraph-experiment/mini-player.html">
                    <meta property="og:video:secure_url"
                      content="https://haku.profpatsch.de/opengraph-experiment/mini-player.html">
                    <meta property="twitter:player"
                      content="https://haku.profpatsch.de/opengraph-experiment/mini-player.html">
                    <meta property="og:video:type" content="text/html">
                    <meta property="og:video:height" content="120">
                    <meta property="og:video:width" content="400">
                    <meta property="twitter:player:height" content="467">
                    <meta property="twitter:player:width" content="350">
                </head>

                <body>
                    <progress-player
                        style="max-width: 400px"
                        src="https://haku.profpatsch.de/public/lecker-bierchen.mp4"
                        coverart="lecker-bierchen.png"
                        artist="Lecker"
                        title="Bierchen">
                    </progress-player>
                    <script src="progress-player.js"></script>
                </body>

                </html>
            '';
          }
          { name = "opengraph-experiment/mini-player.html";
            path = pkgs.writeText "mini-player.html" ''
                <!DOCTYPE html>
                <html>

                <head>
                    <title>Player example</title>
                </head>

                <body style="margin: 0px">
                    <progress-player
                        src="https://haku.profpatsch.de/public/lecker-bierchen.mp4"
                        coverart="lecker-bierchen.png"
                        artist="Lecker"
                        title="Bierchen">
                    </progress-player>
                    <script src="progress-player.js"></script>
                </body>

                </html>
            '';
          }
          { name = "opengraph-experiment/progress-player.js";
            path = "${homeRepo.users.Profpatsch.whatcd-resolver.static-directory}/progress-player.js";
          }
          { name = "opengraph-experiment/lecker-bierchen.png";
            path = ./lecker-bierchen.png;
          }
        ];
        # locations."${youtube2audiopodcastSubdir}/" = {
        #   proxyPass = "http://127.0.0.1:${toString youtube2audiopodcastPort}/";
        # };
        locations."/".root =
          let lojbanistanSrc = pkgs.fetchFromGitHub {
            owner = "lojbanistan";
            repo = "lojbanistan.de";
            rev = "ef02aa8f074d0d5209839cd12ba7a67685fdaa05";
            sha256 = "1hr2si73lam463pcf25napfbk0zb30kgv3ncc0ahv6wndjpsvg7z";
          };
          in pkgs.runCommandLocal "lojbanistan-www" {} ''
            mkdir $out
            echo "coi do" > $out/index.html
            ${pkgs.imagemagick}/bin/convert \
              ${lojbanistanSrc}/design/flag-of-lojbanistan-icon.svg \
              -define icon:auto-resize=64,48,32,16 \
              $out/favicon.ico
          '';
        serverAliases = [ "lojbanistan.de" ];
      };
      virtualHosts.${testHostName} = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://rolery:9999";
          extraConfig = ''
            # forward original host so we can validate mastodon http header signatures
            proxy_set_header Host $host;
          '';
        };
      };
      virtualHosts.${matrixHostName} = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString dentritePort}";
          extraConfig = ''
            # forward original host (necessary?)
            proxy_set_header Host $host;
          '';
        };
      };
    };

    services.caddy = {
      enable = true;

      configFile = pkgs.writeText "Caddyfile" ''
        {
          http_port 9092
          default_bind ${tailscaleAddress}
        }

        :9092 {
          # Serve files from the directory specified by WHATCD_RESOLVER_TRANSMISSION_DOWNLOAD_DIRECTORY
          handle_path /files/* {
            root * ${transmissionDownloadDirectory}
            file_server {
              browse off
            }
          }

          handle_path /static/* {

            root * ${homeRepo.users.Profpatsch.whatcd-resolver.static-directory}
            file_server {
              browse off
            }
          }

          # Reverse proxy from localhost:9092 to localhost:9093
          reverse_proxy * http://${tailscaleAddress}:${toString whatcdResolverPortTailscale}
        }

      '';
    };
    users.users.caddy.extraGroups = [ transmissionDownloadDirectoryGroup ];

    networking = {
      hostName = "haku";

      useNetworkd = true;

      interfaces.enp0s20.useDHCP = true;

      firewall = {
        allowedTCPPorts = [
          80 443
          # 6882
          1337 2342 4223
          60100
        ];
        allowedUDPPorts = [
          60100
        ];

        # warning: Strict reverse path filtering breaks Tailscale exit node use and some subnet routing setups. Consider setting `networking.firewall.checkReversePath` = 'loose'
        checkReversePath = "loose";

        # filter tailscale interface with my firewall
        extraInputRules = ''
          iptables -A INPUT -i tailscale0 -j DROP  # Block incoming traffic via Tailscale
        '';
        interfaces.${tailscaleInterface} = {
          allowedTCPPorts = [
            gonicPortTailscale
            whatcdResolverReverseProxyPortTailscale
            whatcdResolverJaegerPortTailscale
            # sambaPortTailscale
          ];
        };
      };

      # nameservers = [
      #   "62.210.16.6"
      #   "62.210.16.7"
      # ];
    };

    services.tailscale = {
      enable = true;
      interfaceName = tailscaleInterface;
    };

    services.transmission = {
      enable = true;
      user = "transmission";
      group = "transmission";
      settings = {
        rpc-port = 9091;
        peer-port-random-on-start = true;
        peer-port-random-low = 50000;
        peer-port-random-high = 50010;
      };
      openFirewall = true;
      openRPCPort = false;
    };

    services.dendrite =
      let database = {
        connection_string = "postgresql:///dendrite?host=/run/postgresql";
        max_open_conns = 90;
        max_idle_conns = 5;
        conn_max_lifetime = (-1);
      };
      in {
        enable = true;
        httpPort = 8008;

        loadCredential = [ "matrix-key:/var/lib/dendrite/matrix-key" ];
        settings.global.private_key = "$CREDENTIALS_DIRECTORY/matrix-key";
        settings.global.server_name = "decentsoftwa.re";
        settings.global.database = database;
        settings.app_service_api.database = database;
        settings.federation_api.database = database;
        settings.key_server.database = database;
        settings.media_api.database = database;
        settings.mscs.database = database;
        settings.relay_api.database = database;
        settings.room_server.database = database;
        settings.sync_api.database = database;
        settings.user_api.account_database.database = database;
        settings.user_api.device_database.database = database;
        settings.sync_api.search.enabled = true;
        # allow fetching federation server keys via matrix.org (otherwise joining rooms is basically impossible …)
        settings.federation_api.prefer_direct_fetch = false;
        settings.federation_api.key_perspectives = [{
          server_name = "matrix.org";
          keys = [
            { key_id = "ed25519:auto";
              public_key = "Noi6WqcDj0QmPxCNQqgezwTlBKrfqehY1u2FyWP9uYw";
            }
            { key_id = "ed25519:a_RXGa";
              public_key = "l8Hft5qXKn1vfHrg3p4+W8gELQVo8N13JkluMfmn2sQ";
            }
          ];
        }];


        settings.logging = [ { type = "std"; level = "debug"; } ];

        # shared secret config
        openRegistration = false;
        environmentFile = "/var/lib/dendrite/registration_secret";
        settings.client_api.registration_shared_secret = "$REGISTRATION_SHARED_SECRET";
      };
    systemd.services.dendrite = {
      after = [ "postgresql.service" ];
      serviceConfig = {
        User = "dendrite";
        Group = "dendrite";
      };
    };

    services.postgresql = {
      enable = true;
      enableTCPIP = false;
      package = pkgs.postgresql_15;

      ensureDatabases = [
        "dendrite"
      ];
      ensureUsers = [
        {
          name = "dendrite";
          ensureDBOwnership = true;
        }
        {
          name = "root";
          ensureClauses = {
            login = true;
            superuser = true;
          };
        }
      ];
    };
  };

}
