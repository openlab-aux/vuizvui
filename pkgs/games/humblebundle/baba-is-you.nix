{ stdenv, buildGame, fetchHumbleBundle, libGL, makeWrapper }:

buildGame rec {
  name = "baba-is-you-unstable-${version}";
  version = "2019-08-07"; # guessed from aug07 in file name.

  src = fetchHumbleBundle {
    name = "BIY_linux_aug07.tar.gz";
    machineName = "babaisyou_odtbg_linux_oR9qb";
    downloadName = "Download";
    suffix = "tar.gz";
    md5 = "3694afc5579cdaad7448c9744aa8d063";
  };

  buildInputs = [ makeWrapper libGL ];

  sandbox.paths.required = [ "$XDG_DATA_HOME/Baba_Is_You" ];

  installPhase = ''
    mkdir -p "$out/bin" "$out/share/baba-is-you"
    rm -r bin32

    cp -vrt "$out/share/baba-is-you" .
    makeWrapper "$out/share/baba-is-you/bin64/Chowdren" "$out/bin/baba-is-you" \
      --run "cd '$out/share/baba-is-you'"
  '';
}
