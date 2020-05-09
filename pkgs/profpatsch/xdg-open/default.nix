{ pkgs, getBins, importDhall2, writeExecline, dhall, buildDhallPackage }:

let
  lib = pkgs.lib;
  bins = getBins pkgs.libnotify [ "notify-send" ]
      // getBins pkgs.file [ "file" ]
      // getBins pkgs.dmenu [ "dmenu" "dmenu_path" ]
      # TODO: make sure these are the ones from the environment
      // getBins pkgs.emacs [ "emacsclient" ]
      // getBins pkgs.firefox [ "firefox" ]
      // getBins pkgs.lilyterm-git [ "lilyterm" ]
      // getBins pkgs.ranger [ "ranger" ]
      ;

  notify = msg: {
    exe = writeExecline "notify" { readNArgs = 2; } [
            bins.notify-send
            ("\${1} \${2}")
          ];
    args = file: [
      ({String, Variable}: String msg)
      file
    ];
  };

  get-mime-type = writeExecline "get-mime-type" { readNArgs = 1; } [
    bins.file "-E" "--brief" "--mime-type" "$1"
  ];

  compose-mail-to = {
    exe = writeExecline "emacs-mail" { readNArgs = 1; } [
      bins.emacsclient
        "--create-frame"
        "--eval"
        # TODO: this obviously fails if the mail address contains "
        ''(url-mailto (url-generic-parse-url "''${1}"))''
    ];
    args = file: [ file ];
  };

  open-in-browser = {
    exe = bins.firefox;
    args = file: [ file ];
  };

  open-in-editor = {
    exe = bins.emacsclient;
    args = file: [ file ];
  };

  dmenu-list-binaries-and-exec = {
    exe = writeExecline "dmenu-query" { readNArgs = 1; } [
      "backtick" "-in" "cmd" [
        "pipeline" [ bins.dmenu_path ] bins.dmenu
      ]
      "importas" "cmd" "cmd"
      "$cmd" "$1"
    ];
    args = file: [ file ];
  };

  exec-in-terminal-emulator = {exe, args}: {
    exe = bins.lilyterm;
    args = file: [
      ({Variable, String}: String "--execute")
      ({Variable, String}: String exe)
    ] ++ args file;
  };


  Prelude =
    let src = (import ./imports.nix { inherit pkgs; }).Prelude;
    # TODO: bs, make dhall version overridable
    in buildDhallPackage {
      name = "Prelude";
      code = "${src.repo}/${src.mainFile}";
    };

  xdg-open = importDhall2 {
      type = ''
  let Command = { args : < String : Text | Variable : Text >
                  → List < String : Text | Variable : Text >
                , exe : Text }
  in
  ∀(bins : { get-mime-type : Text })
→ ∀(write-dash : Text → Text → Text)
→ ∀(shellEscape : Text → Text)
→ ∀(pkgs : { binary : Text, package : Text } → Text)
→ ∀ ( special
    : { compose-mail-to : Command
      , dmenu-list-binaries-and-exec : Command
      , exec-in-terminal-emulator : ∀ ( args: Command) → Command
      , open-in-browser : Command
      , open-in-editor : Command
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
    ({binary, package}: "${lib.getBin pkgs.${package}}/bin/${binary}")
    {
      inherit
        compose-mail-to
        open-in-browser
        open-in-editor
        dmenu-list-binaries-and-exec
        exec-in-terminal-emulator
        ;
    };

in {
  inherit
    xdg-open
    Prelude
    ;
}
