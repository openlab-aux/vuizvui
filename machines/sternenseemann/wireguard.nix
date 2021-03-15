{ config, pkgs, ... }:

let
  keyDir = "/home/lukas/files/crypto/wireguard";
in

{
  config = {
    networking.wg-quick = {
      interfaces = {
        wg1 = {
          address = [
            "fd03:1337::158/64"
            "10.13.37.158/24"
          ];
          dns = [
            "95.215.19.53"
            "2001:67c:2354:2::53"
          ];
          privateKeyFile = "${keyDir}/njalla-private";

          peers = [
            {
              endpoint = "wg006.njalla.no:51820";
              publicKey = "y0zGwVuzZwKqkNkxP1P9A2Zw9H79cRjTZZlXL8+PrVM=";
              allowedIPs = [
                "0.0.0.0/0"
                "::/0"
              ];
              persistentKeepalive = 60;
            }
          ];
        };
      };
    };
  };
}
