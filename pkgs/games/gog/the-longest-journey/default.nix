{ stdenv, lib, fetchGog, fetchFromGitHub, innoextract, runCommand, buildSandbox
, SDL2, SDL2_net, freetype, libGLU_combined, glew, alsaLib
, libogg, libvorbis, xvfb_run
}:

let
  gameData = runCommand "the-longest-journey-data" rec {
    version = "142";

    # We need a newer version that 1.7, because GOG uses a newer archive
    # format.
    nativeBuildInputs = lib.singleton (innoextract.overrideAttrs (drv: {
      src = fetchFromGitHub {
        owner = "dscharrer";
        repo = "innoextract";
        rev = "4c61bc4da822fc89f2e05bdb2c45e6c4dd7a3673";
        sha256 = "197pr7dzlza4isssvhqhvnrr7wzc9c4b3wnnp03sxpmhviyidln1";
      };
    }));

    data = fetchGog {
      name = "the-longest-journey-${version}.bin";
      productId = 1207658794;
      downloadName = "en1installer1";
      sha256 = "08jg5snlxkzxppq37lsmbhgv9zhwnk1zr4cid5gynzq9b1048rzc";
    };

    setup = fetchGog {
      name = "the-longest-journey-${version}.exe";
      productId = 1207658794;
      downloadName = "en1installer0";
      sha256 = "1h4c2bhf5mhz004r37dwdydl3rhpg1wyr4kyvxxwma7x9grxqyzc";
    };
  } ''
    ln -s "$data" archive-1.bin
    ln -s "$setup" archive.exe
    innoextract -L -m archive.exe
    mkdir "$out"
    mv -t "$out" \
      game.exe gui.ini chapters.ini language.ini x.xarc \
      static global fonts [a-f0-9][a-f0-9]
  '';

  residualvm = stdenv.mkDerivation rec {
    name = "residualvm-${version}";
    version = "20190611";

    src = fetchFromGitHub {
      owner = "residualvm";
      repo = "residualvm";
      rev = "ae1a7fbf6fa6bf88a7adebaedb2cd713d5ccc718";
      sha256 = "1521578jis9s3ilz0ws0msanviyqf70dp54db3d6ssfikc0w3myx";
    };

    patches = [ ./predefined-config.patch ];

    # Current Git version has an --enable-static option so the stdenv setup
    # thinks that there is --disable-static as well, which doesn't exist.
    dontDisableStatic = true;

    enableParallelBuilding = true;
    buildInputs = [
      SDL2 SDL2_net freetype libGLU_combined glew alsaLib
      libogg libvorbis
    ];

    configureFlags = [ "--disable-all-engines" "--enable-engine=stark" ];
  };

  configFile = runCommand "residualvm-stark.ini" {
    nativeBuildInputs = [ xvfb_run residualvm ];
    inherit gameData;
  } ''
    xvfb-run residualvm -p "$gameData" -a
    sed -e '/^\[residualvm\]/a enable_unsupported_game_warning=false' \
      residualvm.ini > "$out"
  '';

  unsandboxed = runCommand "the-longest-journey-${gameData.version}" {
    residualCmd = "${residualvm}/bin/residualvm";
    configArgs = let
      mkXdg = what: fallback: extra: let
        basePath = "\${XDG_${what}_HOME:-$HOME/${fallback}}";
      in "\"${basePath}/the-longest-journey${extra}\"";
    in [
      "--savepath=${mkXdg "DATA" ".local/share" ""}"
      "--config=${mkXdg "CONFIG" ".config" "/settings.ini"}"
    ];
    inherit (stdenv) shell;
    residualArgs = lib.escapeShellArgs [ "--predefined-config=${configFile}" ];
  } ''
    mkdir -p "$out/bin"
    cat > "$out/bin/the-longest-journey" <<EOF
    #!$shell
    exec $residualCmd $residualArgs $configArgs "\$@" tlj-win
    EOF
    chmod +x "$out/bin/the-longest-journey"
  '';

in buildSandbox unsandboxed {
  paths.required = [
    "$XDG_CONFIG_HOME/the-longest-journey"
    "$XDG_DATA_HOME/the-longest-journey"
  ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" ];
}
