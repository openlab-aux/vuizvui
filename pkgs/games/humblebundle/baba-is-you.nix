{ stdenv, buildGame, fetchHumbleBundle, libGL, makeWrapper }:

buildGame rec {
  name = "baba-is-you-unstable-${version}";
  version = "2019-03-31"; # guessed from mar31 in file name.

  # https://dl.humble.com/hempulioy/BIY_linux_mar31.tar.gz
  src = fetchHumbleBundle {
    machineName = "babaisyou_odtbg_linux_oR9qb";
    downloadName = "Download";
    suffix = "tar.gz";
    md5 = "61893c3ead0dc1823115ad83dcb9aee0";
  };

  buildInputs = [ makeWrapper libGL ];

  sandbox.paths.required = [ "$HOME/Baba_Is_You" ];

  installPhase = ''
    mkdir -p "$out/bin" "$out/share/baba-is-you"
    rm -r bin32

    cp -vrt "$out/share/baba-is-you" .
    makeWrapper "$out/share/baba-is-you/bin64/Chowdren" "$out/bin/baba-is-you" \
      --run "cd '$out/share/baba-is-you'"
  '';
}
