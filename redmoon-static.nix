{ config, lib, ... }:

let
  hostMap = {
    spaghetti.ipv4 = "192.168.1.1";
    mmrnmhrm.ipv4 = "192.168.1.20";
    dnyarri.ipv4 = "192.168.1.32";

    mmrnmhrm.device = "enp4s0";
    dnyarri.device = "enp0s25";
  };

  hostcfg = hostMap.${config.networking.hostName};

in {
  # Workaround for my temporary stupid fscking non-IPv6 NAT gateway...
  networking.domain = "redmoon";
  networking.nameservers = [ "212.18.0.5" "212.18.3.5" ];
  networking.defaultGateway = "192.168.1.1";
  networking.interfaces.${hostcfg.device} = {
    ipAddress = hostcfg.ipv4;
    prefixLength = 24;
  };

  networking.extraHosts = let
    hostList = lib.mapAttrsToList (name: { ipv4, ... }: ''
      ${ipv4} ${name} ${name}.redmoon
    '') hostMap;
  in lib.concatStrings hostList;
}
