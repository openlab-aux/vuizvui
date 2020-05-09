{ pkgs, getBins, importDhall2, writeExecline, dhall, buildDhallPackage }:

let
  lib = pkgs.lib;
  bins = getBins pkgs.libnotify [ "notify-send" ]
      // getBins pkgs.file [ "file" ];

  notify = msg: {
    exe = writeExecline "notify" { readNArgs = 2; } [
            bins.notify-send
            ("\${1} \${2}")
          ];
    args = file: [
      ({String, Variable}: String msg)
      ({String, Variable}: Variable file)
    ];
  };

  get-mime-type = writeExecline "get-mime-type" { readNArgs = 1; } [
    bins.file "-E" "--brief" "--mime-type" "$1"
  ];

  Prelude =
    let src = (import ./imports.nix { inherit pkgs; }).Prelude;
    # TODO: bs, make dhall version overridable
    in buildDhallPackage {
      name = "Prelude";
      code = "${src.repo}/${src.mainFile}";
    };

  xdg-open = importDhall2 {
      type = ''
  ∀(bins : { get-mime-type : Text })
→ ∀(write-dash : Text → Text → Text)
→ ∀(shellEscape : Text → Text)
→ ∀(pkgs : { binary : Text, package : Text } → Text)
→ ∀ ( special
    : { compose-mail-to :
          { args : Text → List < String : Text | Variable : Text >, exe : Text }
      , dmenu-list-binaries-and-exec :
          { args : Text → List < String : Text | Variable : Text >, exe : Text }
      , exec-in-terminal-emulator :
            ∀ ( args
              : { args : Text → List < String : Text | Variable : Text >
                , exe : Text
                }
              )
          → { args : Text → List < String : Text | Variable : Text >
            , exe : Text
            }
      , open-in-browser :
          { args : Text → List < String : Text | Variable : Text >, exe : Text }
      , open-in-editor :
          { args : Text → List < String : Text | Variable : Text >, exe : Text }
      }
    )
→ Text
      '';
      root = ./.;
      main = "xdg-open.dhall";
      files = [
        "config.dhall"
        "imports/Prelude/Text/concatSep"
        "imports/Prelude/Text/concatMap"
        "imports/Prelude/Text/concat"
        "imports/Prelude/List/map"
        "imports/Prelude/List/concatMap"
      ];
      deps = [ Prelude ];
    }
    { inherit get-mime-type; }
    pkgs.writers.writeDash
    pkgs.lib.escapeShellArg
    ({binary, package}: "${lib.getBin pkgs.${package}}/bin/${package}")
    {
      compose-mail-to = notify "compose-mail-to";
      dmenu-list-binaries-and-exec = notify "dmenu";
      exec-in-terminal-emulator = exec: notify ("to exec: " + lib.generators.toPretty {} exec);
      open-in-browser = notify "browser";
      open-in-editor = notify "editor";
    };

in {
  inherit
    xdg-open
    Prelude
    ;
}
