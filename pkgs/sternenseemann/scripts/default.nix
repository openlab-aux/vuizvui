{ lib, writeBashBin, writeText, runCommandNoCC, getBins, packageScriptFile
, bash, ruby, cbqn, perl
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
    self.nix-build-on
    self.uni
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
      else
        echo "No backup drive mounted"
      fi
    }

    prune() {
      if mountpoint -q "$MOUNTPOINT"; then
        $BORG prune                       \
          --list                          \
          --glob-archives '{hostname}-*'  \
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
        "prune")
          prune
          ;;
        *)
          die "No such command: $COMMAND"
          ;;
      esac
    else
      mountpoint -q "$MOUNTPOINT" || mount_luks
      backup
      prune
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

  idate = packageScriptFile {
    name = "idate";
    file = ./internet-time.bqn;
    interpreter = cbqn;
  };

  dictcc = packageScriptFile {
    name = "dictcc";
    interpreter = ruby;
  };

  fdate = packageScriptFile {
    name = "fdate";
    interpreter = ruby;
  };

  uni = packageScriptFile {
    name = "uni";
    interpreter = perl;
  };

  # TODO(sterni): allow specifying multiple attrpaths without -A after --
  nix-instantiate-to = writeBashBin "nix-instantiate-to" ''
    set -eu
    if [ $# = 0 ]; then
      printf 'Usage: %s [USER@]HOST ARGS ...\n' "$0" >&2
      exit 101
    fi

    readonly TARGET="$1"
    shift

    compress="zstd -zc -T0"
    decompress="unzstd"
    instantiateFlags=()

    for flag in "$@"; do
      case "$flag" in
        --gzip)
          compress="gzip -c4"
          decompress="gunzip"
          ;;
        *)
          instantiateFlags+=("$flag")
          ;;
      esac
    done

    # TODO(sterni): temporary gcroot?
    drvs="$(nix-instantiate "''${instantiateFlags[@]}")"

    # zstd -z only costs 2% of the time of running the store commands,
    # but reduces the size of the export by 90% in my testing.
    # The output of nix-store --export is not concatenatable, so we
    # can't use xargs(1) and need to hope everything fits into ARG_MAX.
    count="$(nix-store --export $(nix-store --query --requisites $drvs) \
      | $compress \
      | ssh "$TARGET" "$decompress | nix-store --import | wc -l")"
    printf 'copied %s paths\n' "$count" 1>&2

    # Display drv paths after nix-store --import output
    printf '%s\n' "$drvs"
  '';

  # Wrapper around nix-instantiate-to which immediately starts the build on the remote machine
  # TODO(sterni): add option to download the result
  nix-build-on = writeBashBin "nix-build-on" ''
    set -euo pipefail
    if [ $# = 0 ]; then
      printf 'Usage: %s [USER@]HOST ARGS ...\n' "$0" >&2
      exit 101
    fi

    instantiateFlags=()
    realiseFlags=()

    for flag in "$@"; do
      case "$flag" in
        -k|--keep-going|--check|-K|--keep-failed|--dry-run)
          # Gets reinterpreted on the remote host
          realiseFlags+=("$(printf '%q\n' "$flag")")
          ;;
        *)
          instantiateFlags+=("$flag")
          ;;
      esac
    done

    "${self.nix-instantiate-to}/bin/nix-instantiate-to" \
      "''${instantiateFlags[@]}" \
      | ssh "$1" "xargs nix-store ''${realiseFlags[*]} -r"
  '';
}
