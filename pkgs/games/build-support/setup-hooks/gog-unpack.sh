unpackCmdHooks+=(_tryUnpackGogMakeSelf _tryUnpackGogInnoSetup)

_tryUnpackGogMakeSelf() {
  # Make sure it's really a Makeself installer with GOG.com modifications.
  sed -n -e '1,/^\([^#]\| *\)$/ {
    /This script.*Makeself/ {
      n; /GOG\.com/q
    }
    200q1 # This is getting too long, so quit immediately.
    b
  }
  q1' "$curSrc" || return 1

  # The file consists of a shell script at the top, followed by a tarball with
  # the installer and after that tarball, the actual ZIP file with the game
  # data follows. So we need to calculate the offsets accordingly, which
  # luckily are dumped using --dumpconf (we only get the sizes, but we can
  # infer the starting offset for the ZIP file using those).
  local zipfileOffset="$(
    eval "$($SHELL "$curSrc" --dumpconf)"
    declare -i offset=1
    offset+="$(head -n $(($OLDSKIP - 1)) "$curSrc" | wc -c)"
    for fs in $filesizes; do
      offset+="$fs"
    done
    echo "$offset"
  )"

  # Unfortunately bsdtar exits with failure if one of the patterns specified
  # using the --include flag doesn't match. However, if the desktop icon is
  # missing it's not the end of the world, so we need to find another way to
  # make it happen without bsdtar returning a failure.
  #
  # Let's introduce -s, which is used to substitute the paths. While it may
  # sound eligible to be used in conjunction --include, it's only really useful
  # for our case if the inclusion patterns would be matched _after_ the
  # substitiotions. Unfortunately, they're matched before the substitions.
  #
  # So what we do instead is rewrite *everything* that we want to include into
  # a special path "/_/game" and rewrite everything that doesn't begin with /
  # into "skip". We're (ab)using the fact here that files coming from the
  # archive never start with "/", but during the substitutions the leading
  # slash isn't stripped.
  #
  # In the end the resulting paths are normalized, so "/..." will be turned
  # into "./...", so all we need to do in the end is to strip 2 components from
  # the resulting path. This discards every path that has been renamed to
  # "skip".
  tail -c"+$zipfileOffset" "$curSrc" | bsdtar -xf - \
    -s '!^data/noarch/game/\(.*\)$!/_/game/\1!' \
    -s '!^data/noarch/support/icon\.png$!/_/game/xdg-icon.png!' \
    -s '!^[^/].*!skip!' --strip-components=2
}

_tryUnpackGogInnoSetup() {
  innoextract -i "$curSrc" &> /dev/null || return 1

  local -a unpackArgs=()
  if [ -n "$innoExtractOnly" ]; then
    local i
    for i in $innoExtractOnly; do
      unpackArgs+=("--include" "$i")
    done
  fi

  if [ -z "$innoExtractKeepCase" ]; then
    unpackArgs+=("-L")
  fi

  innoextract -s "${unpackArgs[@]}" -m "$curSrc"
}
