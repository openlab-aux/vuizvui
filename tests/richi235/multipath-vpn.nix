{
  name = "multipath-test";

  nodes = let
    common = { lib, ... }: {
      networking.firewall.enable = false;
      networking.useNetworkd = true;
      systemd.network.netdevs."40-eth0".netdevConfig = {
        Name = "eth0";
        Kind = "dummy";
      };
    };

  in {
    client = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 10 ];
      networking.defaultGateway = "42.0.0.1";
      networking.interfaces.eth1.ip4 = lib.singleton {
        address = "42.0.0.2";
        prefixLength = 8;
      };
    };
    mtc = { lib, nodes, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 1 2 10 ];
      networking.interfaces.eth1.ip4 = lib.mkForce (lib.singleton {
        address = "1.0.0.2";
        prefixLength = 8;
      });
      networking.interfaces.eth2.ip4 = lib.mkForce (lib.singleton {
        address = "2.0.0.2";
        prefixLength = 8;
      });
      networking.interfaces.eth3.ip4 = lib.mkForce (lib.singleton {
        address = "42.0.0.1";
        prefixLength = 8;
      });
      systemd.network.networks."40-eth1".routes = lib.singleton {
        routeConfig.Gateway = "1.0.0.1";
        routeConfig.Destination = "10.0.0.0/8";
      };
      systemd.network.networks."40-eth2".routes = lib.singleton {
        routeConfig.Gateway = "2.0.0.1";
        routeConfig.Destination = "11.0.0.0/8";
      };
      vuizvui.services.multipath-vpn.client.enable = true;
      vuizvui.services.multipath-vpn.client.links.vlan1 = {
        interface = "eth1";
        destAddress = "10.0.0.1";
      };
      vuizvui.services.multipath-vpn.client.links.vlan2 = {
        interface = "eth2";
        destAddress = "11.0.0.1";
      };
      vuizvui.services.multipath-vpn.client.tun = {
        ip = "192.168.66.2";
        mask = 24;
      };
      vuizvui.services.multipath-vpn.client.route = {
        network = "0.0.0.0";
        mask = 0;
        gateway = "192.168.66.1";
      };
    };
    relay1 = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 1 3 ];
      networking.useDHCP = false;
      networking.interfaces.eth1.ip4 = lib.mkForce (lib.singleton {
        address = "1.0.0.1";
        prefixLength = 8;
      });
      networking.interfaces.eth2.ip4 = lib.mkForce (lib.singleton {
        address = "10.0.0.2";
        prefixLength = 8;
      });
      systemd.network.networks."40-eth2".routes = lib.singleton {
        routeConfig.Gateway = "10.0.0.1";
        routeConfig.Destination = "10.0.0.0/8";
      };
      networking.nat.enable = true;
      networking.nat.internalInterfaces = [ "eth1" ];
      networking.nat.externalInterface = "eth2";
    };
    relay2 = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 2 4 ];
      networking.useDHCP = false;
      networking.interfaces.eth1.ip4 = lib.mkForce (lib.singleton {
        address = "2.0.0.1";
        prefixLength = 8;
      });
      networking.interfaces.eth2.ip4 = lib.mkForce (lib.singleton {
        address = "11.0.0.2";
        prefixLength = 8;
      });
      systemd.network.networks."40-eth2".routes = lib.singleton {
        routeConfig.Gateway = "11.0.0.1";
        routeConfig.Destination = "11.0.0.0/8";
      };
      networking.nat.enable = true;
      networking.nat.internalInterfaces = [ "eth1" ];
      networking.nat.externalInterface = "eth2";
    };
    mts = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 3 4 5 ];
      networking.interfaces.eth1.ip4 = lib.mkForce (lib.singleton {
        address = "10.0.0.1";
        prefixLength = 8;
      });
      networking.interfaces.eth2.ip4 = lib.mkForce (lib.singleton {
        address = "11.0.0.1";
        prefixLength = 8;
      });
      networking.interfaces.eth3.ip4 = lib.mkForce (lib.singleton {
        address = "6.6.6.42";
        prefixLength = 8;
      });
      networking.nat.enable = true;
      #networking.nat.internalInterfaces = [ "tun0" ];
      networking.nat.externalInterface = "eth3";
      vuizvui.services.multipath-vpn.server.enable = true;
      vuizvui.services.multipath-vpn.server.links.vlan3 = {
        interface = "eth1";
        destAddress = "0"; # XXX
        destPort = 0; # XXX
      };
      vuizvui.services.multipath-vpn.server.links.vlan4 = {
        interface = "eth2";
        destAddress = "0"; # XXX
        destPort = 0; # XXX
      };
      vuizvui.services.multipath-vpn.server.tun = {
        ip = "192.168.66.1";
        mask = 24;
      };
      vuizvui.services.multipath-vpn.server.route = {
        #network = "192.168.0.0";
        #network = "6.0.0.0";
        #mask = 8;
        network = "0.0.0.0";
        mask = 0;
        gateway = "192.168.66.2";
      };
    };
    zs = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 5 ];
      networking.interfaces.eth1.ip4 = lib.mkForce (lib.singleton {
        address = "6.6.6.23";
        prefixLength = 8;
      });
    };
  };

  testScript = { nodes, ... }: ''
    startAll;
    $mtc->waitForUnit("multipath-vpn-client.service");
    $mts->waitForUnit("multipath-vpn-server.service");

    $mtc->sleep(30);

    subtest "test network topology", sub {
      $mtc->succeed("ping -c1 10.0.0.1 >&2");
      $mtc->succeed("ping -c1 11.0.0.1 >&2");
      $mts->succeed("ping -c1 6.6.6.23 >&2");
    };

    subtest "test tunnel connectivity", sub {
      $client->execute("ifconfig >&2");
      $client->execute("ip route >&2");
      $zs->execute("ifconfig >&2");
      $zs->execute("ip route >&2");

      $mtc->execute("ifconfig >&2");
      $mtc->execute("ip route >&2");
      $mts->execute("ifconfig >&2");
      $mts->execute("ip route >&2");

      $mtc->succeed("ping -c1 192.168.66.1 >&2");
      $mtc->succeed("ping -c1 6.6.6.23 >&2");

      #$client->succeed("ping -c1 192.168.66.1 >&2");
      $client->succeed("ping -c1 6.6.6.23 >&2");
    };
  '';
}
