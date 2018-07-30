{ lib, stdenv, buildSandbox, fetchGog, gogUnpackHook, bchunk, p7zip
, scummvm, fetchFromGitHub
, runCommand, xvfb_run

, showItemCosts ? true
, durableArmor ? true
}:

let
  version = "2.1.0.43";

  gameData = stdenv.mkDerivation {
    name = "world-of-xeen-gamedata-${version}";
    inherit version;

    src = fetchGog {
      name = "setup_mm45_${version}.exe";
      productId = 1207661233;
      downloadName = "en1installer1";
      sha256 = "0jv9k5rcapqlk61pawa5l4m34iwllx8j6cfz69gl092h04fvfqki";
    };

    nativeBuildInputs = [ gogUnpackHook ];
    innoExtractOnly = [ "app/game1.gog" "app/music" ];

    patchPhase = ''
      cat > game1.inst <<EOF
      FILE "game1.gog" BINARY
      TRACK 01 MODE1/2352
        INDEX 01 00:00:00
      EOF
    '';

    buildPhase = ''
      ${bchunk}/bin/bchunk game1.gog game1.inst game_cd
      ${p7zip}/bin/7z x game_cd01.iso
    '';

    installPhase = ''
      for i in [Gg][Aa][Mm][Ee]/*.[Cc][Cc]; do
        filename="$(basename "$i")"
        install -vD -m 0644 "$i" "$out/''${filename,,}"
      done

      for i in music/*.ogg; do
        filename="$(basename "$i")"
        install -vD -m 0644 "$i" "$out/''${filename/xeen/track}"
      done
    '';

    doInstallCheck = true;

    installCheckPhase = ''
      ccFileNo="$(ls -1 "$out/"*.cc | wc -l)"
      if [ "$ccFileNo" -ne 3 ]; then
        echo "Expected three .cc files, but got $ccFileNo." >&2
        ls -l "$out"
        exit 1
      fi
      trackFileNo="$(ls -1 "$out/"track[0-9][0-9].ogg | wc -l)"
      if [ "$trackFileNo" -ne 59 ]; then
        echo "Expected 59 track[0-9][0-9].ogg files, but got $trackFileNo." >&2
        ls -l "$out"
        exit 1
      fi
    '';
  };

  latestScummVM = scummvm.overrideAttrs (drv: {
    src = fetchFromGitHub {
      owner = "scummvm";
      repo = "scummvm";
      rev = "ca8b79fa751d1f8eac1e468936cbf1f5d7656674";
      sha256 = "0aa12n3mci7zw2mhh23721ixx0b8zh5463a529s2rkf9wjq751f0";
    };

    configureFlags = (drv.configureFlags or []) ++ [
      "--disable-all-engines" "--enable-engine=xeen"
    ];

    # Current Git version has an --enable-static option so the stdenv setup
    # thinks that there is --disable-static as well, which is not.
    dontDisableStatic = true;
  });

  injectOption = c: o: lib.optionalString c "-e '/^\\[worldof/a ${o}=true'";

  scummVmConfig = runCommand "scummvm-xeen.ini" {
    nativeBuildInputs = [ xvfb_run latestScummVM ];
    inherit gameData;
  } ''
    xvfb-run scummvm -p "$gameData" -a
    sed -e '/^\[scummvm\]/a enable_unsupported_game_warning=false' \
      ${injectOption showItemCosts "ShowItemCosts"} \
      ${injectOption durableArmor "DurableArmor"} \
      scummvm.ini > "$out"
  '';

  unsandboxed = runCommand "word-of-xeen-${version}" {
    scummVmCmd = "${latestScummVM}/bin/scummvm";
    dataHome = "\"\${XDG_DATA_HOME:-$HOME/.local/share}/xeen\"";
    inherit (stdenv) shell;
    scummVmArgs = lib.concatMapStringsSep " " lib.escapeShellArg [
      "-c" scummVmConfig
    ];
  } ''
    mkdir -p "$out/bin"
    cat > "$out/bin/xeen" <<EOF
    #!$shell
    exec $scummVmCmd $scummVmArgs --savepath=$dataHome "\$@" worldofxeen-cd
    EOF
    chmod +x "$out/bin/xeen"
    cat "$out/bin/xeen"
  '';

in buildSandbox unsandboxed {
  paths.required = [ "$XDG_DATA_HOME/xeen" ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" ];
}
