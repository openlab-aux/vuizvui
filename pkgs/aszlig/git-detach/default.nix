{ writeScriptBin, stdenv, git, coreutils, patch }:

writeScriptBin "git-detach" ''
  #!${stdenv.shell}

  if [ $# -le 0 -o "$1" = "--help" -o "$1" = "-h" ]; then
      echo "Usage: $0 COMMAND [ARGS...]" >&2
      echo >&2
      echo "Run COMMAND in a clean Git working directory" >&2
      echo "without untracked files and .git directory." >&2
      exit 1
  fi

  diffToHead="$("${git}/bin/git" diff HEAD)"

  if tmpdir="$("${coreutils}/bin/mktemp" -d git-detach.XXXXXXXXXX)"; then
    trap "rm -rf '${"\${tmpdir//\\'/\\'\\\\\\'\\'}"}'" EXIT
    "${git}/bin/git" archive --format=tar HEAD | (
      set -e
      basedir="$tmpdir/$("${coreutils}/bin/basename" "$(pwd)")"
      mkdir "$basedir"
      cd "$basedir"
      tar x
      if [ -n "$diffToHead" ]; then
        echo "$diffToHead" | "${patch}/bin/patch" -s -p1
      fi
      exec "$@"
    )
    exit $?
  else
    echo "Unable to create temporary directory!" >&2
  fi
''
