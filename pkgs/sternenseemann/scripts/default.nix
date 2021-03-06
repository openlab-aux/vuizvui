# TODO(sterni): port to dash
{ lib, writeBashBin, writeText, getBins
, libnotify
, borgbackup, cryptsetup
, ghostscript
, openssl
, perl, mandoc
, shakti
, gpp
}:

let
  backupExcludes = writeText "backup-excludes" ''
    /home/lukas/.cache
    /home/lukas/.config/chromium
    /home/lukas/.config/google-chrome
    /home/lukas/.config/discord
    /home/lukas/.stack
    /home/lukas/.notmuch
    /home/lukas/.local/share/
    /home/lukas/Mail/.notmuch
    /home/lukas/.npm
    /home/lukas/.gem
    /home/lukas/.npm
    /home/lukas/.meteor
    /home/lukas/tmp
    /home/lukas/Videos/tmp
    /home/lukas/.mozilla
    /home/lukas/.cabal
    /home/lukas/.bundle
    /home/lukas/.opam
    /home/lukas/files/serverkram/minecraft/
  '';

  bins = (getBins shakti [ "k-repl" ])
      // (getBins gpp [ "gpp" ])
      // (getBins perl [ "perl" ])
      // (getBins cryptsetup [ "cryptsetup" ])
      // (getBins borgbackup [ "borg" ])
      // (getBins mandoc [ "man" ])
      // (getBins openssl [ "openssl" ])
      // (getBins perl [ "pod2man" ])
      // (getBins ghostscript [ "gs" ])
      // (getBins libnotify [ "notify-send" ])
      ;

in

lib.fix (self: {
  default = [
    self.borg-wrapper
    self.lowview
    self.pdfcombine
    self.pdfrange
    self.xmpp-notify
    self.certprint
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

  opam-env = writeBashBin "opam-env" ''
    nix-shell -p pkgs.opam \
      pkgs.m4 pkgs.gnumake pkgs.binutils pkgs.gcc pkgs.gmp pkgs.glib pkgs.pkg-config \
      --command ". $HOME/.opam/opam-init/init.sh; eval \`opam config env\`; return" \
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

  podview = writeBashBin "podview" ''
    ${bins.pod2man} "$1" | ${bins.man} -l
  '';

  xmpp-notify = writeBashBin "xmpp-notify" ''
    if [[ $3 != "connect" ]]; then
      ${bins.notify-send} "jackline" "IM received"
      printf '\a'
    fi
  '';

  k-gpp = writeBashBin "k-gpp" ''
    if [[ $# > 1 ]]; then
      echo "$0: max one argument allowed" >&2
      exit 1
    fi

    if [[ ! -z "$1" ]]; then
      preprocessed="$(mktemp --suffix=".k")"
      echo "gpp $1 -o $preprocessed" >&2
      ${bins.gpp} "$1" -o "$preprocessed"
      ${bins.k-repl} "$preprocessed"
      status=$?
      rm "$preprocessed"
      exit $status
    else
      ${bins.k-repl}
    fi
  '';
})
