{
  name = "heinrich";

  nodes = let
    common = { lib, ... }: {
      networking.useNetworkd = true;
      systemd.network.netdevs."40-eth0".netdevConfig = {
        Name = "eth0";
        Kind = "dummy";
      };
    };
  in {
    heinrich = {
      imports = [ common (import ../machines {}).labnet.heinrich.config ];
      virtualisation.vlans = [ 1 8 14 ];
      heinrich.internalInterface = "eth1";
      heinrich.externalInterface = "eth2";
    };
    hotelturm = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 8 ];
      networking.useDHCP = false;
      networking.interfaces.eth1.ip4 = lib.singleton {
        address = "10.11.77.16";
        prefixLength = 24;
      };
    };
    moritz = { lib, ... }: {
      imports = [ common ];
      virtualisation.vlans = [ 14 ];
      networking.useDHCP = false;
      networking.interfaces.eth1.ip4 = lib.singleton {
        address = "192.168.0.1";
        prefixLength = 24;
      };
    };
    client = {
      imports = [ common ];
      virtualisation.vlans = [ 1 ];
    };
  };

  testScript = ''
    startAll;
    $heinrich->waitForUnit("dnsmasq.service");
    $client->waitForUnit("network-interfaces.target");
    $client->waitForUnit("network.target");

    $client->succeed("ip addr >&2");
  '';
}
