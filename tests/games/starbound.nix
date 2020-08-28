{ pkgs, nixpkgsPath, ... }:

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
    with client.nested("${description}"):
      client.screenshot("before_${escapeScreenshot name}")
      client.succeed("${pkgs.xdotool}/bin/xdotool '${xdoFile}'")
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
      virtualisation.memorySize = 2047;
      networking.interfaces.eth1.ipAddress = "192.168.0.1";
      networking.interfaces.eth1.prefixLength = 24;
      networking.firewall.enable = false;
    };

    client = { pkgs, ... }: {
      imports = [
        "${nixpkgsPath}/nixos/tests/common/x11.nix"
      ];
      virtualisation.memorySize = 2047;
      environment.systemPackages = [
        pkgs.vuizvui.games.humblebundle.starbound
      ];
      networking.interfaces.eth1.ipAddress = "192.168.0.2";
      networking.interfaces.eth1.prefixLength = 24;
      networking.firewall.enable = false;
    };
  };

  testScript = ''
    # fmt: off
    server.wait_for_unit("starbound.service")

    with client.nested("waiting for client to start up"):
      client.wait_for_x()
      client.succeed("starbound >&2 &")
      client.wait_for_text('(?i)options')

    ${clickAt "join-game" 100 560}
    client.wait_for_text('(?i)select')
    ${clickAt "new-character" 460 220}
    client.wait_for_text('(?i)randomise')
    ${clickAt "create-character" 600 625}
    client.wait_for_text('(?i)select')
    ${clickAt "use-character" 460 220}
    client.wait_for_text('(?i)ser[vu]er')

    ${clickAt "server-address" 460 322}
    ${typeText "server-address" "192.168.0.1"}

    ${clickAt "server-account" 490 354}
    ${typeText "server-account" "alice"}

    ${clickAt "server-password" 490 386}
    ${typeText "server-password" "secret"}

    ${clickAt "join-server" 495 420}

    client.wait_for_text('(?i)graduation')
    client.sleep(30)
    client.screenshot("client")
  '';
}
