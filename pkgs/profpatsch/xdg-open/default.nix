{ pkgs, getBins,
importDhall2,
writeExecline,
buildDhallPackage,
runExeclineLocal,
writeRustSimple,
netencode-rs,
record-get,
el-exec,
lazy-packages
}:

let
  lib = pkgs.lib;
  bins = getBins pkgs.libnotify [ "notify-send" ]
      // getBins pkgs.file [ "file" ]
      // getBins pkgs.coreutils [ "printf" "ln" "echo" ]
      // getBins pkgs.fdtools [ "multitee" ]
      // getBins pkgs.s6 [ "s6-ioconnect" ]
      // getBins pkgs.s6-portable-utils [ "s6-test" ]
      // getBins pkgs.s6-networking [ "s6-tcpclient" ]
      // getBins pkgs.netcat-openbsd [ "nc" ]
      // getBins pkgs.dmenu [ "dmenu" "dmenu_path" ]
      # TODO: make sure these are the ones from the environment
      // getBins pkgs.emacs [ "emacsclient" ]
      // getBins pkgs.firefox [ "firefox" ]
      // getBins pkgs.ranger [ "ranger" ]
      // getBins pkgs.khal [ "khal" ]
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

  # TODO: interactive adding? Don’t want to add all ics files to my calendar
  add-to-calendar = {
    exe = writeExecline "add-to-calendar" { readNArgs = 1; } [
      "if" [
        bins.khal
          "import"
          "--batch"
          # the private calendar is called calendar
          "--include-calendar" "calendar"
          "$1"
      ]
      "systemctl" "--user"
        "start"
        # defined as a user service (TODO: config variable?)
        "calendar-sync"
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
    exe = pkgs.vuizvui.profpatsch.tvl.users.Profpatsch.alacritty;
    args = file: [
      ({Variable, String}: String "--execute")
      ({Variable, String}: String exe)
    ] ++ args file;
  };

  fetch-command-on-demand = cmd: lazy-packages.mkWrapper {
    package = cmd;
  };

  fetch-http-url-mime = {
    exe = writeExecline "fetch-http-url-mime" { readNArgs = 1; } [
      "pipeline" [ read-headers-and-follow-redirect "$1" ]
      record-get [ "content-type" ]
      printenv "content-type"
    ];
    args = file: [ file ];
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
→ ∀(pkgsOnDemand : { binary : Text, package : Text } → Text)
→ ∀ ( special
    : { compose-mail-to : Command
      , dmenu-list-binaries-and-exec : Command
      , exec-in-terminal-emulator : ∀ ( args: Command) → Command
      , fetch-http-url-mime : Command
      , open-in-browser : Command
      , open-in-editor : Command
      , notify : Text -> Command
      , add-to-calendar : Command
      }
    )
→ Text
      '';
      root = ./.;
      main = "xdg-open.dhall";
      files = [
        "config.dhall"
        "types.dhall"
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
    ({binary, package}: "${lazy-packages.mkWrapper {
      package = (lib.getBin pkgs.${package});
    }}/bin/${binary}")
    {
      inherit
        compose-mail-to
        open-in-browser
        fetch-http-url-mime
        fetch-command-on-demand
        open-in-editor
        dmenu-list-binaries-and-exec
        exec-in-terminal-emulator
        notify
        add-to-calendar
        ;
    };

  httparse = pkgs.buildRustCrate {
    pname = "httparse";
    version = "1.3.4";
    crateName = "httparse";
    sha256 = "0dggj4s0cq69bn63q9nqzzay5acmwl33nrbhjjsh5xys8sk2x4jw";
  };

  stderr-tee = writeExecline "stderr-tee" {} [
    "pipeline" [ bins.multitee "0-1,2" ] "$@"
  ];

  stderr-prog = writeExecline "stderr-prog" {} [
    "if" [ "fdmove" "-c" "1" "2" bins.echo "$@" ]
    "$@"
  ];

  http-request = writeExecline "http-request" { } [
    "importas" "-i" "protocol" "protocol"
    "ifelse" [ bins.s6-test "$protocol" "=" "http" ] [ (http-https-request false) ]
    "ifelse" [ bins.s6-test "$protocol" "=" "https" ] [ (http-https-request true) ]
    eprintf "protocol \${protocol} not supported"
  ];

  http-https-request = isHttps: writeExecline "http-https-request" { } ([
    "multisubstitute" [
      "importas" "-ui" "port" "port"
      "importas" "-ui" "host" "host"
      "importas" "-ui" "path" "path"
    ]
    "pipeline" [
      bins.printf (lib.concatStringsSep "\r\n" [
      ''HEAD %s HTTP/1.1''
      ''Host: %s''
      ''User-Agent: lol''
      ''Accept: */*''
      ""
      ""
      ]) "$path" "$host"
    ]
    stderr-tee
    stderr-prog
    "pipeline" ([
    bins.nc
  ] ++ lib.optional isHttps "-c" ++ [
      "-v" "-N" "$host" "$port"
      ])
    stderr-tee
    "cat"
  ]);

  printenv = writeRustSimple "printenv" {}
    (pkgs.writeText "printenv.rs" ''
      use std::io::Write;
      use std::os::unix::ffi::OsStrExt;
      fn main() -> std::io::Result<()> {
        let usage = || {
          eprintln!("usage: printenv VAR");
          std::process::exit(1)
        };
        let mut args = std::env::args_os();
        let _ = args.next().unwrap_or_else(usage);
        let var = args.next().unwrap_or_else(usage);
        match std::env::var_os(&var) {
          None => {
            let mut err = std::io::stderr();
            err.write_all("env variable ".as_bytes())?;
            err.write_all(var.as_bytes())?;
            err.write_all(" does not exist\n".as_bytes())?;
          },
          Some(stuff) => std::io::stdout().write_all(stuff.as_bytes())?
        }
        Ok(())
      }
    '');

  assert-printf = writeExecline "assert-printf" { argMode = "env"; } [
    "ifelse" [ "runblock" "2" ]
    [ "runblock" "-r" "2" ]
    "fdmove" "-c" "1" "2"
    "runblock" "1" bins.printf
  ];

  as-stdin = writeExecline "as-stdin" { readNArgs = 1; } [
    "pipeline" [ printenv "$1" ] "$@"
  ];

  read-headers-and-follow-redirect = writeExecline "read-headers-and-follow-redirect" { readNArgs = 1; }
    (let go = writeExecline "go" {} [
      "pipeline" [ http-request ]
      "pipeline" [ read-http ]
      record-get [ "status" "status-text" "headers" ]
      "importas" "-ui" "status" "status"
      # TODO: a test util for netencode values
      "ifelse" [ bins.s6-test "$status" "=" "n6:301," ]
      # retry the redirection location
      [ as-stdin "headers"
        record-get [ "Location" ]
        "importas" "-ui" "Location" "Location"
        "export" "host" "$Location"
        "if" [ "echo" "redirected to \${Location}" ]
        # save path, which would be overwritten by mini-url
        "importas" "old-path" "path"
        mini-url "$Location"
        "export" "path" "$old-path"
        "$0"
      ]
      printenv "headers"
    ];
    in [
      mini-url "$1"
      go
    ]);

  read-http = writeRustSimple "read-http" {
    dependencies = [ httparse netencode-rs ];
    buildInputs = [ pkgs.skalibs ];
  } ./read-http.rs;

  mini-url = writeRustSimple "mini-url" {
    dependencies = [ el-exec ];
    buildInputs = [ pkgs.skalibs ];
    release = false;
    # buildTests = true;
    verbose = true;
  } ./mini-url.rs;

  eprintf = writeExecline "eprintf" {} [
    "fdmove" "-c" "1" "2" bins.printf "%s" "$@"
  ];

in {
  inherit
    xdg-open
    Prelude
    read-headers-and-follow-redirect
    mini-url
    assert-printf
    as-stdin
    printenv
    ;
}
