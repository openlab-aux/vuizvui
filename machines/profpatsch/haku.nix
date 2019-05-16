{ config, pkgs, lib, ... }:

let
  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

  warpspeedPort = 1338;

  myKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDNMQvmOfon956Z0ZVdp186YhPHtSBrXsBwaCt0JAbkf/U/P+4fG0OROA++fHDiFM4RrRHH6plsGY3W6L26mSsCM2LtlHJINFZtVILkI26MDEIKWEsfBatDW+XNAvkfYEahy16P5CBtTVNKEGsTcPD+VDistHseFNKiVlSLDCvJ0vMwOykHhq+rdJmjJ8tkUWC2bNqTIH26bU0UbhMAtJstWqaTUGnB0WVutKmkZbnylLMICAvnFoZLoMPmbvx8efgLYY2vD1pRd8Uwnq9MFV1EPbkJoinTf1XSo8VUo7WCjL79aYSIvHmXG+5qKB9ed2GWbBLolAoXkZ00E4WsVp9H philip@nyx";

in

{
  imports = [
    ./base-server.nix
  ];

  config = {

    # TODO abstract out
    nix.maxJobs = 2;
    vuizvui.modifyNixPath = false;
    nix.nixPath = [
      "vuizvui=/root/vuizvui"
      "nixpkgs=/root/nixpkgs"
      # TODO: nicer?
      "nixos-config=${pkgs.writeText "haku-configuration.nix" ''
        (import <vuizvui/machines>).profpatsch.haku.config
      ''}"
    ];

    system.autoUpgrade = {
      enable = true;
      channel = "https://headcounter.org/hydra/channel/custom/openlab/vuizvui/channels.machines.profpatsch.haku";
    };

    vuizvui.user.profpatsch.server.sshPort = 7001;

    boot.loader.grub.device = "/dev/sda";
    # VPN support
    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];

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

    environment.systemPackages = with pkgs; [
      rtorrent                          # bittorrent client
      mktorrent                         # torrent file creator
      pkgs.vuizvui.profpatsch.warpspeed # trivial http file server
    ];

    users.users = {
      root.openssh.authorizedKeys.keys = [ myKey ];

      rtorrent = {
        isNormalUser = true;
      };
      vorstand = {
        isNormalUser = true;
        openssh.authorizedKeys.keys = [ myKey
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCUgS0VB5XayQobQfOi0tYeqpSSCXzftTKEyII4OYDhuF0/CdXSqOIvdqnWQ8933lPZ5234qCXCniIlRJpJQLBPJdJ7/XnC6W37asuft6yVYxTZnZat8edCuJETMvwZJZNttxHC04k3JPf9RMj25luICWabICH5XP9Mz3GoWSaOz7IOm7jiLQiF3UtiFOG06w76d3UfcIVbqjImwWv8nysphi9IQfL0XgC24zNE6LSeE7IN5xTOxoZxORQGsCEnFNCPevReNcSB0pI9xQ1iao7evaZkpzT4D4iQ/K7Ss8dsfFWN30NPMQS5ReQTUKtmGn1YlgkitiYTEXbMjkYbQaQr daniel@shadow"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCtfWeIH7YZpWUUOZ3oC5FB2/J+P3scxm29gUQdVij/K0TuxW1yN/HtcvrO1mwSshS6sNZ2N6/Kb6+kuGyx1mEnaFt87K5ucxC7TNqiURh4eeZE1xX7B5Ob8TVegrBxoe+vcfaoyxn7sUzgF719H0aYC7PP6p3AIbhq3hRLcvY26u9/gZ39H79A71wCunauvpcnpb+rqyJMN6m2YoeOcoloe7wUDI8Xw5dUetHpNKn9k1vzS16CdwP4pAKI8aBtdNK7ZojVMe9LfBG8HHPr9K+cwcaxQuXkFBJzrfrtBCfQwrgWppsu/W/kGBs1ybku2bOFI5UXJBnsraXQqr1NLIfL phj@phj-X220"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDj8dla7nOE7RSho2/9LAn+DANYkB1BmMoNryzTQ5mUJWukf5coCc+aNJcXYeu5dSTEicW2qQuD8mt8SDI5Qzv4oSpIYEsd0j4eW/BlC5XYd+4jS7Hfk/a1mJjMG7jdvOUtK3lLtrKaHxVUUjqdxKzzFBZlPov6FgHSJ//h1HxreV/Y0jL94qSvK39FZde5xlV/wQBvpglrMNu7FFWqyeKrOZ7U8D70scFliIuPok/02iQ31P+ncUfV3XrFyJodQq8J3hYEorGVKp3nNM1zaLlg8uqHk18Zt0GFnEAClBrC13yjM0jpMvaMyuXMaWuKeqsBZeUyaSo1j6BNsW/bFjiJ thomas-glamsch@gmx.de"
        ];
      };
    };

    systemd.services.warpspeed =
      let user = config.users.users.rtorrent;
      in {
        description = "internally served public files (see nginx)";
        wantedBy = [ "default.target" ];
        serviceConfig.WorkingDirectory = "${user.home}/public";
        script = "${pkgs.vuizvui.profpatsch.warpspeed}/bin/warpspeed ${toString warpspeedPort}";
        serviceConfig.User = config.users.users.rtorrent.name;
      };

    services.nginx = {
      enable = true;
      virtualHosts."haku.profpatsch.de" = {
        forceSSL = true;
        enableACME = true;
        locations."/pub/" = {
          proxyPass = "http://127.0.0.1:${toString warpspeedPort}/";
        };
        locations."/".root =
          let lojbanistanSrc = pkgs.fetchFromGitHub {
            owner = "lojbanistan";
            repo = "lojbanistan.de";
            rev = "ef02aa8f074d0d5209839cd12ba7a67685fdaa05";
            sha256 = "1hr2si73lam463pcf25napfbk0zb30kgv3ncc0ahv6wndjpsvg7z";
          };
          in pkgs.runCommand "lojbanistan-www" {} ''
            mkdir $out
            echo "coi do" > $out/index.html
            ${pkgs.imagemagick}/bin/convert \
              ${lojbanistanSrc}/design/flag-of-lojbanistan-icon.svg \
              -define icon:auto-resize=64,48,32,16 \
              $out/favicon.ico
          '';
        serverAliases = [ "lojbanistan.de" ];
      };
    };

    networking = {
      hostName = "haku";
      firewall = {
        allowedTCPPorts =
          [ 80 443
            # transmission
            6882
          ];
      };
      nameservers = [
        "62.210.16.6"
        "62.210.16.7"
      ];
    };
  };
}
