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
            "fd03:1337::216/64"
            "10.13.37.216/24"
          ];
          dns = [
            "95.215.19.53"
            "2001:67c:2354:2::53"
          ];
          privateKeyFile = "${keyDir}/njalla-private";

          peers = [
            {
              endpoint = "wg009.njalla.no:51820";
              publicKey = "5qO6a8iN7eU7/vQkG/0I31Aks1WNbAeJHwJ+ds1llGY=";
              allowedIPs = [
                "0.0.0.0/0"
                "::/0"
              ];
              persistentKeepalive = 25;
            }
          ];
        };
      };
    };
  };
}
