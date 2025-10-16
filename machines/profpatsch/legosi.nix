{ modulesPath, config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  hostname = "legosi";

  gpgPublicKeyring = pkgs.runCommandLocal "keyring" {} ''
    export GNUPGHOME=.
    ${pkgs.gnupg}/bin/gpg --import ${../../pkgs/profpatsch/profpatsch.de/key.asc}
    cp ./pubring.kbx $out
  '';
  gpgPublicKeyId = "4ACFD7592710266E18CEBB28C5CFD08B22247CDF";

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";
  qwerkyKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM3ORvMbdHaJfgPgMhWTqgVrP1L7kkvuETQpzl0IjP2G tc@windoof";

  tailscaleInterface = "tailscale0";
  tailscaleAddress = "100.89.52.54";

  xandikosPort = 2345;
  siteServerPort = 2346;
  gititPort = 2347;
  indexServerPort = 2348;
in {
  imports = [
    ./base-server.nix
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  config = {
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
            gititPort
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

    vuizvui.user.profpatsch.services.gitit = {
      enable = true;
      stateDirName = "gitit-blog";
      config = {
        port = gititPort;
        # disable-registration = "no";
      };
    };

    security.acme.acceptTerms = true;
    security.acme.defaults.email = "mail@profpatsch.de";

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      virtualHosts.${"profpatsch.de"} = {
        forceSSL = true;
        enableACME = true;
        locations."/index.html" = {
          proxyPass = "http://localhost:${toString indexServerPort}";
        };
        # this is a horrible workaround for having a proxy-pass for the index.html
        # while still using the static files from the site server
        extraConfig = ''
          location = / {
            try_files $uri $uri/ /index.html;
          }
        '';
        locations."/" = {
          root = pkgs.vuizvui.profpatsch.websiteStatic;
        };
        # gpg-wks-client --print-wkd-hash mail@profpatsch.de
        locations."/.well-known/openpgpkey/hu".root = pkgs.linkFarm "well-known-pgp-keys" [
          { name = ".well-known/openpgpkey/hu/dizb37aqa5h4skgu7jf1xjr4q71w4paq";
            path = ./../../pkgs/profpatsch/profpatsch.de/key.asc;
          }
        ];
        # pass the rest to the site server (TODO: make static!)
        locations."/notes" = {
          proxyPass = "http://localhost:${toString gititPort}";
        };
        locations."/projects" = {
          proxyPass = "http://localhost:${toString siteServerPort}";
        };
        locations."/posts" = {
          proxyPass = "http://localhost:${toString siteServerPort}";
        };
        locations."/mlp/" = {
          alias = "/sync/www/mlp/";
        };
      };
      virtualHosts.${"decentsoftwa.re"} = {
        forceSSL = true;
        enableACME = true;
        locations."/".root =
          let
            index = pkgs.writeText "index.html" ''
              <!DOCTYPE html>
              <html>
                <head>
                  <meta charset="utf-8">
                  <title>decentsoftwa.re</title>
                  <!--
                  prevent favicon request, based on answers in
                  https://stackoverflow.com/questions/1321878/how-to-prevent-favicon-ico-requests
                  TODO: create favicon
                  -->
                  <link rel="icon" href="data:,">
                </head>
                <body>
                  <script>
	                function createFavicon() {
	                   const canvas = document.createElement('canvas');
	                   const context = canvas.getContext('2d');
	                   canvas.width = 32; // Set the size of the icon
	                   canvas.height = 32;

	                   context.fillStyle = 'rgba(0,0,0,0)'; // Background color
	                   context.fillRect(0, 0, canvas.width, canvas.height);
	                   context.font = '15px sans-serif'; // Font and size
	                   // Text color based on color sheme
	                   context.fillStyle = window.matchMedia('(prefers-color-scheme: dark').matches ? 'white' : 'black';
	                   context.fillText('DCT', 0, 24); // Draw the Unicode character

	                   const link = document.createElement('link');
	                   link.rel = 'icon';
	                   link.href = canvas.toDataURL('image/png');
	                   document.head.appendChild(link);
	                }
	                createFavicon();
	                // switch favicon color on dark mode change
	                window.matchMedia('(prefers-color-scheme: dark').addEventListener('change', (v) => {createFavicon()})
	              </script>
                </body>
              </html>
            '';
            
            warcry = pkgs.writeText "warcry.txt" ''
              No modes
              No build steps
			  No frameworks
			  No hot new shit
			  No BDFLs
			  No blessed domains
			  No free speech maximalism
            '';
          in pkgs.runCommandLocal "decentsoftware-www" {} ''
            mkdir $out
            cp ${index} $out/index.html
            cp ${warcry} $out/warcry.txt
          '';

        locations."/.well-known/matrix/".root = pkgs.linkFarm "well-known-decentsoftwa.re-matrix" [
          { name = ".well-known/matrix/server";
            path = pkgs.writers.writeJSON "matrix-server-well-known" {
              "m.server" = "matrix.decentsoftwa.re:443";
            };
          }
        ];

        # account.conversations.im XMPP certificate signature
        locations."/.well-known/posh".root = pkgs.linkFarm "well-known-xmpp-client-thingy" [
          { name = ".well-known/posh/xmpp-client.json";
            path = pkgs.writers.writeJSON "conversations.im-xmpp-client.json" {
              fingerprints = [ { "sha-256" = "pSWZpTkHmNZdGk6CjsaNXXexlCAu+NcXg8MIzUB8tbU="; } ];
              "expires" = 3600;
            };
          }
        ];

        # make XMPP clients know about the conversations.im BOSH endpoint
        locations."/.well-known/host-meta" = {
          proxyPass = "https://conversations.im/.well-known/host-meta";
          # https://jsxc.readthedocs.io/en/latest/getting-started/requirements.html#nginx
          extraConfig = ''
            proxy_set_header Host $proxy_host;
            tcp_nodelay on;
          '';
        };
      };
    };

    systemd.services.index-server = {
      description = "index server for my website";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = pkgs.vuizvui.profpatsch.index-server {
          port = toString indexServerPort;
        };
        Restart = "always";
        RestartSec = "1s";
        DynamicUser = true;
      };
    };

    systemd.services.notes-server = {
      description   = "notes for my website";
      wantedBy      = [ "multi-user.target" ];
      after         = [ "network.target" ];
      serviceConfig = {
        ExecStart = pkgs.vuizvui.profpatsch.homeRepo.users.Profpatsch.blog.site-server {
          port = toString siteServerPort;
          # TODO: css has to be adjusted for articles
          # cssFile = pkgs.vuizvui.profpatsch.concatenatedCss;
          cssFile = null;
        };
        Restart = "always";
        RestartSec = "1s";
        DynamicUser = true;
      };
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
