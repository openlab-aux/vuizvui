{ config, pkgs, lib, ... }:

with lib;

let
  deps = with pkgs.perlPackages; rec {
    IOInterface = buildPerlPackage {
      name = "IO-Interface-1.09";
      src = fetchurl {
        url = mirror://cpan/authors/id/L/LD/LDS/IO-Interface-1.09.tar.gz;
        sha256 = "0fkizbclng7jaxkwj9cr2wby34r45mazb0yrq87fdq5i5v2q2gp6";
      };
      buildInputs = [ ModuleBuild ];
      preConfigure = "touch Makefile.PL";
      buildPhase = "perl Build.PL --prefix=$out; ./Build build";
      installPhase = "./Build install";
      checkPhase = "./Build test";
    };

    IOPipely = buildPerlPackage {
      name = "IO-Pipely-0.005";
      src = fetchurl {
        url = mirror://cpan/authors/id/R/RC/RCAPUTO/IO-Pipely-0.005.tar.gz;
        sha256 = "0x1fkwbkbkhxf0cvz08yj24hm9c775i1xx8khlqfwiibrgsnqfz3";
      };
    };

    ModuleBuild = buildPerlPackage {
      name = "Module-Build-0.4211";
      src = fetchurl {
        url = mirror://cpan/authors/id/L/LE/LEONT/Module-Build-0.4211.tar.gz;
        sha256 = "1c5hfhajr963w4mdjivsc7yz4vf4pz1rrfch5a93fbac1x2mr58h";
      };
      doCheck = false;
    };

    POE = buildPerlPackage {
      name = "POE-1.366";
      src = fetchurl {
        url = mirror://cpan/authors/id/R/RC/RCAPUTO/POE-1.366.tar.gz;
        sha256 = "08qmb45clkjw2ni9dl5y1fa4ifrinvbvvcgh7r20ls32frw034xl";
      };
      buildInputs = [ POETestLoops ];
      propagatedBuildInputs = [ IOPipely IOTty POETestLoops ];
    };

    POETestLoops = buildPerlPackage {
      name = "POE-Test-Loops-1.360";
      src = fetchurl {
        url = mirror://cpan/authors/id/R/RC/RCAPUTO/POE-Test-Loops-1.360.tar.gz;
        sha256 = "0yx4wsljfmdzsiv0ni98x6lw975cm82ahngbwqvzv60wx5pwkl5y";
      };
    };

    POEWheelUDP = pkgs.buildPerlPackage {
      name = "POE-Wheel-UDP-0.02";
      src = fetchurl {
        url = mirror://cpan/authors/id/H/HA/HACHI/POE-Wheel-UDP-0.02.tar.gz;
        sha256 = "0d611cqpmq7svmxq6pbjb59b97x5zh2z4lc11f8zjmci98nag2g6";
      };
      propagatedBuildInputs = [ POE ];
    };
  };

  linkOptions = { name, ... }: {
    options = {
      interface = mkOption {
        type = types.str;
        description = ''
          IP address or interface name to connect to the relay.
        '';
      };

      sourcePort = mkOption {
        type = types.int;
        default = 11218;
        description = ''
          Local UDP port to use for connecting to the other endpoint.
        '';
      };

      destAddress = mkOption {
        type = types.str;
        description = ''
          Remote UDP host or IP of the other endpoint.
        '';
      };

      destPort = mkOption {
        type = types.int;
        default = 11218;
        description = ''
          Remote UDP port the other endpoint is listening.
        '';
      };

      ratio = mkOption {
        type = types.int;
        default = 1;
        description = ''
          Defines how many packets the remote endpoint is getting in relation to
          the other defined links.
        '';
      };
    };
  };

  commonOptions = {
    links = mkOption {
      default = {};
      type = types.attrsOf (types.submodule linkOptions);
      description = ''
        Links used to connect to the remote endpoint (server).
      '';
    };

    tun.ip = mkOption {
      type = types.str;
      description = ''
        IP address of the TUN interface used for communicating to/from the
        outside of the tunnel.
      '';
    };

    tun.mask = mkOption {
      type = types.int;
      description = ''
        Network prefix length to use for the TUN interface.
      '';
    };

    tun.mtu = mkOption {
      type = types.int;
      default = 1500;
      description = ''
        Maximum transfer unit for the TUN interface.
      '';
    };

    route.network = mkOption {
      type = types.str;
      description = ''
        Network address of the auto-enabled route.
      '';
    };

    route.mask = mkOption {
      type = types.int;
      description = ''
        Network prefix length of the auto-enabled route.
      '';
    };

    route.gateway = mkOption {
      type = types.str;
      description = ''
        Gateway address of the auto-enabled route.
      '';
    };
  };

  clientOptions = commonOptions // {
    enable = mkEnableOption "Multipath VPN Client";
  };

  serverOptions = commonOptions // {
    enable = mkEnableOption "Multipath VPN Server";
  };

  genConfig = name: cfg: mkIf cfg.enable (let
    attrs = if name == "client" then {
      descName = "Client";
    } else if name == "server" then {
      descName = "Server";
    } else throw "Invalid multipath VPN config mode";

    mpvpn = pkgs.stdenv.mkDerivation rec {
      name = "multipath-vpn";

      src = pkgs.fetchFromGitHub {
        owner = "richi235";
        repo = name;
        rev = "51729f7bb24b5361c90469c60f67df0c8b4e2371";
        sha256 = "1p2i1m649nhrylqz2grc5nxwgzqq1rnwkzk7iipdxabx2164ahaq";
      };

      configFile = pkgs.writeText "mpvpn.conf" ''
        ${concatStringsSep "\n" (mapAttrsToList (
          name: attrs: concatStringsSep "\t" [
            "link" name attrs.interface
            (toString attrs.sourcePort)
            attrs.destAddress
            (toString attrs.destPort)
            (toString attrs.ratio)
          ]
        ) cfg.links)}

        ${concatStringsSep "\t" [
          "local" cfg.tun.ip (toString cfg.tun.mask) (toString cfg.tun.mtu)
        ]}

        ${concatStringsSep "\t" [
          "route" cfg.route.network (toString cfg.route.mask) cfg.route.gateway
        ]}
      '';

      buildPhase = "true";
      buildInputs = [
        pkgs.makeWrapper pkgs.perl
        deps.POEWheelUDP deps.IOInterface
      ];
      installPhase = ''
        mkdir -p "$out/bin"

        sed -e "s,/etc/multivpn.cfg,$configFile," \
            -e 's/detect+handle_local_ip_change/handle_local_ip_change/g' \
            vpn_client_and_server.pl > "$out/bin/multipath-vpn"

        chmod +x "$out/bin/multipath-vpn"
        wrapProgram $out/bin/multipath-vpn --set PERL5LIB $PERL5LIB
      '';
    };
  in {
    systemd.services."multipath-vpn-${name}" = {
      description = "Multipath VPN ${attrs.descName}";
      after = [ "network-interfaces.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.iptables pkgs.nettools pkgs.iproute pkgs.bridge-utils ];
      serviceConfig.ExecStart = "@${mpvpn}/bin/multipath-vpn multipath-vpn";
    };
  });

in {
  options.vuizvui.services.multipath-vpn.client = commonOptions // {
    enable = mkEnableOption "Multipath VPN Client";
  };

  options.vuizvui.services.multipath-vpn.server = commonOptions // {
    enable = mkEnableOption "Multipath VPN Server";
  };

  config = mkMerge [
    (genConfig "client" config.vuizvui.services.multipath-vpn.client)
    (genConfig "server" config.vuizvui.services.multipath-vpn.server)
  ];
}
