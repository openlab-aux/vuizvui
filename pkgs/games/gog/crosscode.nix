{ lib, buildGame, fetchGog, makeWrapper, nwjs }:

buildGame rec {
  name = "crosscode-${version}";
  version = "1.1.0";

  src = fetchGog {
    productId = 1252295864;
    downloadName = "en3installer0";
    sha256 = "1rqf1vlg151hxy5f9nwldmb4l3853dmvcf7fiakab8vzsmjmldlm";
  };

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = ''
    substituteInPlace package.json --replace assets/ ""

    # Remove Greenworks (Steamworks integration)
    rm -r assets/modules
  '';

  installPhase = ''
    mkdir -p "$out/share" "$out/bin"
    cp -r assets "$out/share/crosscode"
    install -vD -m 0644 package.json "$out/share/crosscode/package.json"

    makeWrapper ${lib.escapeShellArg "${nwjs}/bin/nw"} "$out/bin/crosscode" \
      --run "cd '$out/share/crosscode'" --add-flags .
  '';

  sandbox.paths.required = [ "$XDG_CONFIG_HOME/CrossCode" ];
}
