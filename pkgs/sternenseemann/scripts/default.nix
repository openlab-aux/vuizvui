{ lib, writeBashBin, writeText, runCommandNoCC, getBins
, bash, ruby
, makeWrapper
, borgbackup, cryptsetup
, ghostscript
, poppler_utils
, openssl
, mandoc
, msr-tools
, sternenseemann
}:

let
  self = sternenseemann.scripts;

  packageScriptFile =
    { name
    , file ? ./. + "/${name}"
    , interpreter
    , isShell ? false
    , runtimeDependencies ? []
    }:

    let
      binPath = lib.makeBinPath runtimeDependencies;
    in

    runCommandNoCC name {
      buildInputs = [ interpreter ];
      nativeBuildInputs = [ makeWrapper ];
    } (''
      install -Dm755 "${file}" "$out/bin/${name}"
      patchShebangs "$out/bin/${name}"
    '' + lib.optionalString (runtimeDependencies != []) (
      if isShell then ''
        sed -i \
          -e '2i export PATH="'${lib.escapeShellArg binPath}':$PATH"' \
          "$out/bin/${name}"
      '' else ''
        wrapProgram "$out/bin/${name}" \
          --prefix PATH : ${lib.escapeShellArg binPath}
      ''
    ));

  backupExcludes = writeText "backup-excludes" ''
    /home/lukas/.cache
    /home/lukas/.config/chromium
    /home/lukas/.config/google-chrome
    /home/lukas/.config/discord
    /home/lukas/.stack
    /home/lukas/.notmuch
    /home/lukas/.local/share/
    /home/lukas/.local/state/cabal
    /home/lukas/.local/state/wireplumber
    /home/lukas/.local/state/pipewire
    /home/lukas/Mail/.notmuch
    /home/lukas/.npm
    /home/lukas/.gem
    /home/lukas/.npm
    /home/lukas/.meteor
    /home/lukas/tmp
    /home/lukas/.cabal
    /home/lukas/.bundle
    /home/lukas/.opam
    /home/lukas/files/serverkram/minecraft/
    /home/lukas/Videos/movies
    /home/lukas/Videos/dok
    /home/lukas/Videos/series
    /home/lukas/Videos/dl
    /home/lukas/src/cpp/llvm-project
  '';

  bins = (getBins cryptsetup [ "cryptsetup" ])
      // (getBins borgbackup [ "borg" ])
      // (getBins mandoc [ "man" ])
      // (getBins openssl [ "openssl" ])
      // (getBins ghostscript [ "gs" ])
      // (getBins poppler_utils [ "pdftotext" ])
      ;

in

{
  default = [
    self.lowview
    self.pdfcombine
    self.pdfrange
    self.ls2count
    self.nix-instantiate-to
  ];

  borg-wrapper = writeBashBin "borg-wrapper" ''
    BACKUP_DRIVE="$1"
    MAPPER=backup
    MOUNTPOINT=/mnt
    export BORG_REPO=/mnt/fliewatuet-backup
    BORG=${bins.borg}
    CRYPTSETUP=${bins.cryptsetup}

    die() {
      echo "$1" >> /dev/stderr

      exit 1
    }

    mount_luks() {
      echo "Opening LUKS disk"

      $CRYPTSETUP open --type luks "$BACKUP_DRIVE" $MAPPER || die "Could not open LUKS disk"
      echo "Mounting LUKS disk via mapper"
      mount "/dev/mapper/$MAPPER" "$MOUNTPOINT" || die "Could not mount disk"

    }

    umount_luks() {
      sync

      if mountpoint -q "$MOUNTPOINT"; then
        umount "$MOUNTPOINT"
      fi

      if [[ -e "/dev/mapper/$MAPPER" ]]; then
        $CRYPTSETUP close --type luks "$MAPPER"
      fi
    }


    backup() {
      if mountpoint -q "$MOUNTPOINT"; then
        echo "Starting Backup"

        $BORG create                      \
          --verbose                       \
          --filter AME                    \
          --list                          \
          --stats                         \
          --show-rc                       \
          --compression lz4               \
          --exclude-caches                \
          --exclude-from ${backupExcludes}\
          ::'{hostname}-{now}'            \
          /etc/nixos                      \
          /home                           || die "Backup failed"

        $BORG prune                       \
          --list                          \
          --prefix '{hostname}-'          \
          --show-rc                       \
          --keep-daily 7                  \
          --keep-weekly 4                 \
          --keep-monthly 6                || die "Pruning failed"
      else
        echo "No backup drive mounted"
      fi
    }

    COMMAND="$2"

    if [[ -n "$COMMAND" ]]; then
      case "$COMMAND" in
        "mount")
          mount_luks
          ;;
        "umount")
          umount_luks
          ;;
        "backup")
          backup
          ;;
        *)
          die "No such command: $COMMAND"
          ;;
      esac
    else
      mountpoint -q "$MOUNTPOINT" || mount_luks
      backup
      umount_luks
    fi
  '';

  certprint = writeBashBin "certprint" ''
    OPENSSL=${bins.openssl}
    server=$1
    port=$2

    shift
    shift

    $OPENSSL s_client -connect "$server:$port" $@ < /dev/null | \
      $OPENSSL x509 -fingerprint -sha256 -noout -in /dev/stdin
  '';

  lowview = writeBashBin "lowview" ''
    lowdown -Tterm $@ | less -R
  '';

  pdfcombine = writeBashBin "pdfcombine" ''
    out=$1
    shift

    ${bins.gs} -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile="$out" $@
  '';

  pdfrange = writeBashBin "pdfrange" ''
    # this function uses 3 arguments:
    #     $1 is the first page of the range to extract
    #     $2 is the last page of the range to extract
    #     $3 is the input file
    #     output file will be named "inputfile_pXX-pYY.pdf"
    ${bins.gs} -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
       -dFirstPage="''${1}" \
       -dLastPage="''${2}" \
       -sOutputFile="''${3%.pdf}_p''${1}-p''${2}.pdf" \
       "''${3}"
  '';

  ls2count = writeBashBin "ls2count" ''
    set -euo pipefail
    for f in "$@"; do
      ${bins.pdftotext} "$f" - | tr -d '[:space:]' | wc -m
    done
  '';

  disable-bd-prochot = packageScriptFile {
    name = "disable-bd-prochot";
    file = ./disable-bd-prochot.sh;
    isShell = true;
    interpreter = bash;
    runtimeDependencies = [
      msr-tools
    ];
  };

  fdate = packageScriptFile {
    name = "fdate";
    interpreter = ruby;
  };

  # TODO(sterni): companion script that actually also starts the build on the remote host
  nix-instantiate-to = writeBashBin "nix-instantiate-to" ''
    set -eu
    if [ $# = 0 ]; then
      printf 'Usage: %s [USER@]HOST ARGS ...\n' "$0" >&2
      exit 101
    fi

    readonly TARGET="$1"
    shift
    nix-instantiate "$@" | xargs nix-copy-closure -s --gzip --to "$TARGET"
  '';
}
