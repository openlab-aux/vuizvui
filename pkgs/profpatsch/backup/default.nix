{ pkgs, getBins, writeExecline, backtick }:

let

  bins = getBins pkgs.duplicity [ "duplicity" ]
      // getBins pkgs.pass [ "pass" ]
      // getBins pkgs.coreutils [ "printf" "echo" ];

  gpgKeyId = "4ACFD7592710266E18CEBB28C5CFD08B22247CDF";

  fetchSecretIntoEnv = writeExecline "fetch-secret-into-env" { readNArgs = 2; } [
    "backtick" "-in" "$1" [
      bins.pass "show" "$2"
    ]
    "$@"
  ];

  debugExec = msg: writeExecline "debug-exec" {} [
    "if" [
      "fdmove" "-c" "1" "2"
      "if" [ bins.printf "%s: " msg ]
      "if" [ bins.echo "$@" ]
    ]
    "$@"
  ];

  # TODO: create ncdu script by removing trailing slashes
  # from excludes

  exclude-home-dirs = [
    # archived not backupped
    "Downloads/"
    "Music/"
    "Documents/"
    "Dropbox/"
    "Pictures/"
    "videos/"
    "games/"
    # local tmp dir
    "tmp/"
    # big uninteresting stuff
    ".cache/"
    ".local/share/Steam/"
    ".local/share/Trash/"
    ".config/chromium/Default/Service?Worker/"
    ".config/chromium/Default/IndexedDB/"
    ".config/chromium/Default/Local?Storage/"
    ".config/chromium/Default/Application?Cache/"
    "Android/"
    # no idea why Code is in .config …
    ".config/Code/"
    ".stack/"
    ".cargo/"
    ".mozilla/firefox/*.default/storage/"
    ".cabal/"
    ".go/"
    ".rustup/"
    ".android/"
    ".vscode/"
    ".vagrant.d/"
    ".minecraft/"
    ".npm/"
    ".gem/"
    # consistently updating caches
    ".Mail/.notmuch/xapian/"
  ];

  exclude-code-build-dirs = [
    ".stack-work/"
    "target/"
    "node_modules/"
    "dist/"
  ];

  commonOptions = root:
    pkgs.lib.concatMap (e: [ "--exclude" "${root}/${e}" ]) exclude-home-dirs ++
    pkgs.lib.concatMap (e: [ "--exclude" "${root}/kot/**/${e}" ]) exclude-code-build-dirs ++ [
    # "--dry-run"
    "--progress"
    "--verbosity" "info"
    "--asynchronous-upload"
    "--full-if-older-than" "60D"
    "--num-retries" "3"
    "--use-agent"
    # TODO "--log-fd"
  ];

  callDuplicity = name: argv: writeExecline name {} ([
    # used by duplicity for all kinds of backends
    # TODO: pass the right password depending on the application-key
    # fetchSecretIntoEnv "FTP_PASSWORD" "backups/backblaze.com/application-keys/profpatsch-restore/applicationKey"
    fetchSecretIntoEnv "FTP_PASSWORD" "backups/backblaze.com/application-keys/duplicity-main-backup/applicationKey"
    (debugExec "duplicity call")
    bins.duplicity
  ] ++ argv);

  duplicity-verify = { name, local, write, read }: callDuplicity "duplicity-verify-${name}"
    ([ "verify" ]
    ++ (commonOptions local) ++ [
      "--name" name
      read
      local
    ]);

  duplicity-restore = { name, local, write, read }: callDuplicity "duplicity-restore-${name}"
    ([ "restore" ]
    ++ (commonOptions local) ++ [
      "--name" name
      # extra flags
      "$@"
      read
      local
    ]);

  duplicity-list = { name, local, write, read }: callDuplicity "duplicity-list-${name}"
    ([ "list-current-files" ]
    ++ (commonOptions local) ++ [
      "--name" name
      read
    ]);

  duplicity-incremental = { name, local, write, read }: callDuplicity "duplicity-incremental-${name}"
    ([ "incremental" ]
    ++ (commonOptions local) ++ [
      "--encrypt-sign-key" gpgKeyId
      "--name" name
      local
      write
    ]);

  home = {
    name = "home";
    local = "/home/philip";
    write = "b2://000efe88f7148a00000000001@profpatsch-main-backup/home";
    read = "b2://000efe88f7148a00000000004@profpatsch-main-backup/home";
  };

  legosi = {
    name = "legosi-root";
    local = "/home/philip/tmp/legosi-root";
    write = "b2://000efe88f7148a00000000003@profpatsch-legosi/";
    read = "b2://000efe88f7148a00000000004@profpatsch-legosi/";
  };

  incremental-home = duplicity-incremental home;
  verify-home = duplicity-verify home;
  list-home = duplicity-list home;
  verify-legosi = duplicity-verify legosi;
  restore-legosi = duplicity-restore legosi;
  list-legosi = duplicity-list legosi;

in {
  inherit
    incremental-home
    verify-home
    list-home
    verify-legosi
    restore-legosi
    list-legosi
    ;
}
