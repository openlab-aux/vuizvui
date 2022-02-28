{ pkgs, getBins, writeExecline, runExeclineLocal, toNetstringKeyVal, ... }:

let
  inherit (pkgs) lib;
  bins = getBins pkgs.nodejs [ "node" ]
      // getBins pkgs.coreutils [ "echo" "ln" "mkdir" ]
      // getBins pkgs.dhall-json [ "json-to-dhall" ]
      // getBins pkgs.xorg.xrandr [ "xrandr" ]
      ;

  writeNodejs = {
    name,
    # an attrset of node dependency name to source directory;
    # will be made into a node_modules directory and set as `NODE_PATH`.
    dependencies
  }:
    let
      node_modules = runExeclineLocal "${name}-node_modules" {
        stdin = toNetstringKeyVal dependencies;
      } [
        "importas" "out" "out"
        "if" [ bins.mkdir "$out" ]
        "forstdin" "-o" "0" "-Ed" "" "dep"
        "multidefine" "-d" "" "$dep" [ "name" "source" ]
        "if" [ bins.echo "\${name} - \${source}" ]
        bins.ln "-sT" "\${source}" "\${out}/\${name}"
      ];

    in pkgs.writers.makeScriptWriter {
      interpreter = writeExecline "nodejs-with-modules" {} [
        "export" "NODE_PATH" node_modules
        bins.node "$@"
      ];
    } name;

  dhall-typecheck = {
    name,
    dhallType,
    recordsLoose ? false
  }:
    writeExecline name {} ([
      bins.json-to-dhall
    ]
    ++ lib.optional recordsLoose "--records-loose"
    ++ [
      dhallType
    ]);

  parse = writeNodejs {
    name = "xrandr-parse";
    dependencies = {
      xrandr-parse =
        (pkgs.fetchFromGitHub {
          owner = "lionep";
          repo = "xrandr-parse";
          rev = "a35bfd625c1b0834aa94f136cf8282b25624b3e6";
          sha256 = "0c8mfsvgg76ia2i9gsgdwy567xzba5274xpj8si37yah5qpp8dkm";
        });
    };
  } ''
    var parse = require('xrandr-parse');
    var exec = require('child_process').exec;

    exec('xrandr', function (err, stdout) {
        var query = parse(stdout);
        console.log(JSON.stringify(query, null, 2));
    });
  '';

  type = pkgs.writeText "type.dhall" ''
    let Resolution = {
      height: Text,
      width: Text,
      rate: Double
    }
    let Monitor =
      < NotConnected : {}
      | Connected : {
          connected: Bool,
          modes: List Resolution,
          index: Natural,
          native: Resolution
      }
      | Active : {
          modes: List Resolution,
          index: Natural,
          native: Resolution,
          current: Resolution
      } >

    in List {
      mapKey: Text,
      mapValue: Monitor
    }
  '';


  two-monitor-setup = writeExecline "test" {} [
    "backtick" "-Ei" "json" [ parse ]
    "if" [
      "pipeline" [ bins.echo "$json" ]
      "redirfd" "-w" "1" "/dev/null"
      (dhall-typecheck {
        name = "typecheck-xrandr";
        dhallType = type;
        recordsLoose = true;
      })
    ]
    "pipeline" [ bins.echo "$json" ]
    two-monitor-setup-script
  ];


  two-monitor-setup-script = pkgs.writers.writePython3 "xrandr-two-monitor-setup" {} ''
    import json
    import sys
    import os

    # TODO: use netencode for input
    monitors = json.load(sys.stdin)

    connected = {
      k: v for k, v in monitors.items()
      if 'connected' in v and v['connected']
    }

    if 'eDP1' not in connected:
        print("could not find eDP1 (laptop screen)", file=sys.stderr)
        sys.exit(1)

    if len(connected) != 2:
        print("only know how to configure two monitors")
        sys.exit(1)

    eDP1 = connected['eDP1']

    # laptop screen is active
    assert('current' in eDP1)

    # how far the laptop screen should be offset to end
    # at the bottom of the monitor
    h_offset = 0
    for k, v in connected.items():
        if k == 'eDP1':
            assert('current' in v)

        else:
            h = int(v['native']['height'])
            h_offset = h - int(eDP1['native']['height'])
            external_monitor = (k, v)
            # can’t handle bigger laptop screens atm
            assert(h_offset >= 0)

    xrandr_command = [
        "--verbose",

        "--output", external_monitor[0],
        "--primary",
        "--mode", "{}x{}".format(
            external_monitor[1]['native']['width'],
            external_monitor[1]['native']['height']
        ),
        "--pos", "{}x{}".format(
            # offset by the laptop size to the right
            eDP1['native']['width'],
            # monitor starts at 0
            0
        ),

        "--output", "eDP1",
        "--mode", "{}x{}".format(
            eDP1['native']['width'],
            eDP1['native']['height'],
        ),
        "--pos", "{}x{}".format(
            # laptop is the leftmost screen
            0,
            # but offset to the bottom so that it always aligns on the left bottom
            h_offset
        ),
    ]

    os.execvp(
        "${bins.xrandr}",
        ["${bins.xrandr}"]
        + xrandr_command
    )
  '';

in {
  inherit
    parse
    two-monitor-setup
    ;
}
