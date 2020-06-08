{ buildGame, fetchGog, makeWrapper, fixFmodHook, SDL2 }:

buildGame rec {
  name = "into-the-breach-${version}";
  version = "1.2.24";

  src = fetchGog {
    productId = 2004253604;
    downloadName = "en3installer0";
    sha256 = "1m9jbgczjdhkgznd51qivh95d8k2wvdq8wx2vlwaid0iqmnf7p0n";
  };

  nativeBuildInputs = [ makeWrapper fixFmodHook ];
  buildInputs = [ SDL2 ];

  installPhase = ''
    install -vD Breach "$out/libexec/into-the-breach/breach"

    for name in libfmod.so.10 libfmodstudio.so.10; do
      install -vD "linux_x64/$name" "$out/libexec/into-the-breach/$name"
    done

    mkdir -p "$out/share/into-the-breach"
    cp -rt "$out/share/into-the-breach" data maps resources scripts shadersOGL

    mkdir -p "$out/bin"
    makeWrapper "$out/libexec/into-the-breach/breach" "$out/bin/breach" \
      --run "cd '$out/share/into-the-breach'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/IntoTheBreach" ];
}
