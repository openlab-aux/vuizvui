{ config, lib, ... }:

with lib;

let
  routes = {
    moritz = {
      id = 14;
      address = "192.168.0.12";
      prefixLength = 24;
      gateway = "192.168.0.1";
      destination = "144.76.143.122";
    };

    hotelturm = {
      id = 8;
      address = "10.11.77.5";
      prefixLength = 24;
      gateway = "10.11.77.16";
      destination = "10.11.7.0/24";
    };
  };

  internalIf = config.heinrich.internalInterface;
  externalIf = config.heinrich.externalInterface;

  mkRouteConfig = name: cfg: {
    key = "routes-${name}";

    networking.vlans.${name} = {
      inherit (cfg) id;
      interface = externalIf;
    };

    networking.interfaces.${name}.ip4 = singleton {
      inherit (cfg) address prefixLength;
    };

    systemd.network.networks."40-${name}".routes = singleton {
      routeConfig.Gateway = cfg.gateway;
      routeConfig.Destination = cfg.destination;
    };
  };

in {
  imports = mapAttrsToList mkRouteConfig routes;

  options.heinrich = {
    internalInterface = mkOption {
      type = types.str;
      default = "enp7s0";
      description = ''
        The internal network interface where Heinrich is serving DHCP and DNS
        requests.
      '';
    };

    externalInterface = mkOption {
      type = types.str;
      default = "enp5s0";
      description = ''
        The external network interface where Heinrich is connected to the
        internet.
      '';
    };
  };

  config = {
    networking.useDHCP = false;
    networking.interfaces.${externalIf}.ip4 = mkForce [];
    networking.interfaces.${internalIf}.ip4 = lib.singleton {
      address = "172.16.0.1";
      prefixLength = 24;
    };

    services.dnsmasq.enable = true;
    services.dnsmasq.resolveLocalQueries = false;
    services.dnsmasq.extraConfig = ''
      dhcp-range=172.16.0.100,172.16.0.254,12h

      dhcp-option=3,172.16.0.1 # Gateway
      dhcp-option=6,172.16.0.1 # DNS-server

      local=/openlab.lan/
      domain=openlab.lan

      dhcp-leasefile=/var/db/dnsmasq/dhcp.leases
    '';

    systemd.services.dnsmasq-pre = {
      description = "Pre-Init DNSMasq";
      before = [ "dnsmasq.service" ];
      wantedBy = [ "multi-user.target" ];
      script = ''
        mkdir -p /var/db/dnsmasq
        chown dnsmasq:nogroup /var/db/dnsmasq
      '';
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
    };

    users.motd = ''
      0. Never touch a running system.
      1. Dokumentiere alle trotz 0 erfolgten Änderungen im Github-Repo:
         https://github.com/openlab-aux/labnetz-doku
      2. Mit großer Macht geht große Verantwortung einher.
      3. So weit!
      4. ...
      5. Reisst dir Hannes den Arsch auf, wenn Du die Punkte 0-2 ignorierst.
    '';

    networking.useNetworkd = true;
    networking.firewall.enable = false;
    networking.nat.enable = true;
    networking.nat.externalIP = routes.hotelturm.address;
    networking.nat.externalInterface = "hotelturm";
    networking.nat.internalIPs = [ "172.16.0.1/24" ];
    networking.nat.internalInterfaces = [ internalIf ];

    /* TODO!
    services.openvpn.enable = true;
    services.openvpn.servers.heinrich.config = ''
      dev tun0
      remote 144.76.143.122
      ifconfig 10.9.8.2 10.9.8.1
      secret /etc/openvpn/priv.key

      comp-lzo

      keepalive 10 60
      ping-timer-rem
      persist-tun
      persist-key

      route 0.0.0.0 0.0.0.0
    '';
    */
  };
}
