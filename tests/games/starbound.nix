{ pkgs, ... }:

let
  xdo = { name, description, xdoScript }: let
    xdoFile = pkgs.writeText "${name}.xdo" ''
      search --onlyvisible --class starbound
      windowfocus --sync
      windowactivate --sync
      ${xdoScript}
    '';
    escapeScreenshot = pkgs.lib.replaceStrings ["-"] ["_"];
  in ''
    $client->nest("${description}", sub {
      $client->screenshot("before_${escapeScreenshot name}");
      $client->succeed("${pkgs.xdotool}/bin/xdotool '${xdoFile}'");
    });
  '';

  clickAt = name: x: y: xdo {
    name = "click-${name}";
    description = "clicking on ${name} (coords ${toString x} ${toString y})";
    xdoScript = ''
      mousemove --window %1 --sync ${toString x} ${toString y}
      click --repeat 10 1
    '';
  };

  typeText = name: text: xdo {
    name = "type-${name}";
    description = "typing `${text}' into Starbound";
    xdoScript = ''
      type --delay 200 '${text}'
    '';
  };

in {
  name = "starbound";

  enableOCR = true;

  nodes = {
    server = {
      vuizvui.services.starbound = {
        enable = true;
        # Use a different dataDir than the default to make
        # sure everything is still working.
        dataDir = "/var/lib/starbound-test";
        users.alice.password = "secret";
      };
      virtualisation.memorySize = 1024;
      networking.interfaces.eth1.ipAddress = "192.168.0.1";
      networking.interfaces.eth1.prefixLength = 24;
      networking.firewall.enable = false;
    };

    client = { pkgs, ... }: {
      imports = [
        "${import ../../nixpkgs-path.nix}/nixos/tests/common/x11.nix"
      ];
      virtualisation.memorySize = 2047;
      environment.systemPackages = [ pkgs.vuizvui.games.steam.starbound ];
      networking.interfaces.eth1.ipAddress = "192.168.0.2";
      networking.interfaces.eth1.prefixLength = 24;
      networking.firewall.enable = false;
    };
  };

  testScript = ''
    $server->waitForUnit("starbound.service");

    $client->nest("waiting for client to start up", sub {
      $client->waitForX;
      $client->succeed("starbound >&2 &");
      $client->waitForText(qr/options/i);
    });

    ${clickAt "multiplayer" 100 460}
    $client->waitForText(qr/select/i);
    ${clickAt "new-character" 460 170}
    $client->waitForText(qr/species/i);
    ${clickAt "create-character" 600 525}
    $client->waitForText(qr/select/i);
    ${clickAt "use-character" 460 170}
    $client->waitForText(qr/ser[vu]er/i);

    ${clickAt "server-address" 460 272}
    ${typeText "server-address" "192.168.0.1"}

    ${clickAt "server-account" 490 304}
    ${typeText "server-account" "alice"}

    ${clickAt "server-password" 490 336}
    ${typeText "server-password" "secret"}

    ${clickAt "join-server" 495 370}

    $client->waitForText(qr/q[uv]est/i);
    ${xdo {
      name = "close-quest-dialog";
      description = "closing the quest dialog window";
      xdoScript = ''
        key Escape
      '';
    }}
    ${xdo {
      name = "move-right";
      description = "moving to the right of the ship";
      xdoScript = ''
        keydown d
        sleep 10
        keyup d
      '';
    }}

    $client->screenshot("client");
  '';
}
