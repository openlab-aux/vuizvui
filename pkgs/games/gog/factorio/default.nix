{ lib, buildGame, fetchGog, alsa-lib, libGL, libpulseaudio, xorg
, python3, python3Packages, runCommand
}:

buildGame rec {
  name = "factorio-${version}";
  version = "1.0.0";

  src = fetchGog {
    productId = 1238653230;
    sha256 = "1n6a4qbp9i161jdn0f28ih7zc1blraqk0ypi3pizkc7ddc7bb7ja";
  };

  nativeBuildInputs = [ python3 python3Packages.r2pipe ];

  buildInputs = [
    alsa-lib libGL libpulseaudio xorg.libICE xorg.libSM xorg.libX11
    xorg.libXcursor xorg.libXext xorg.libXinerama xorg.libXrandr
  ];

  postPatch = "python3 ${lib.escapeShellArg (runCommand "patcher" {
    src = lib.sourceFilesBySuffices ./. [ ".py" ];
    nativeBuildInputs = [
      python3Packages.mypy
      python3Packages.flake8
      python3Packages.pytest
      python3Packages.hypothesis
      python3Packages.r2pipe
    ];
  } ''
    pytest "$src" -o "cache_dir=$PWD"
    mypy "$src"
    flake8 "$src"
    install -vD -m 0644 "$src/patch.py" "$out"
  '')} bin/x64/factorio";

  installPhase = ''
    install -vD bin/x64/factorio "$out/bin/factorio"
    mkdir -p "$out/share"
    cp -rT data "$out/share/factorio"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/factorio" ];
}
