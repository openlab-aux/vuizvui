{ stdenv, lib, buildSandbox, fetchGog, fetchzip, innoextract, SDL2, SDL2_mixer
, bchunk, p7zip, alsaLib, writeText, makeWrapper, libGL

# For static recompilation
, fetchFromGitHub, scons, judy, python, nasm, autoreconfHook

, language ? "en"
}:

let
  version = "1.6.1";

  staticRecompilerSource = fetchFromGitHub {
    owner = "M-HT";
    repo = "SR";
    rev = "albion_v${version}";
    sha256 = "0yspgssfk5xbrs5krq0sin561rgb0fva4hk7mlxlcrvs0xpqf5z8";
  };

  mkPatchedWildMidi = variant: stdenv.mkDerivation {
    name = "${variant}-0.2.3.5patched";
    src = "${staticRecompilerSource}/midi-libs/${variant}-0.2.3.5svn";
    nativeBuildInputs = [ autoreconfHook ];
    buildInputs = [ alsaLib ];
    patches = [ ./wildmidi-build-fixes.patch ];
    postPatch = ''
      sed -i -e '/^CFLAGS/s/-pedantic//' configure.ac
      sed -i -e '/^wildmidi_libs *=/s!\$(top_builddir)/src/!!' src/Makefile.am
    '';
  };

  compileMidiPlugin = variant: let
    commonDrvAttrs = rec {
      name = "midi-plugin-${variant}";
      soname = "midi-${variant}";
      sourceFile = "${soname}.c";
      midiLib = "WildMidi";

      src = "${staticRecompilerSource}/midi-plugins";

      buildInputs = lib.singleton (mkPatchedWildMidi variant);

      timidityCfg = let
        gusPatches = lib.overrideDerivation (fetchzip {
          url = "http://sebt3.openpandora.org/pnd/timidity_midi_installer.pnd";
          sha256 = "0nznac8lxcbj0fwbg0njlnh3ysa3d3c5i24n2cw0yv5yqji4cdsb";
          stripRoot = false;
        }) (lib.const { unpackCmd = "${p7zip}/bin/7z x \"$curSrc\""; });
      in writeText "timidity-albion.cfg" ''
        dir ${gusPatches}/eawpats
        source ${gusPatches}/eawpats/sounds.cfg
      '';

      postPatch = ''
        sed -i -e 's!getenv("TIMIDITY_CFG")!"'"$timidityCfg"'"!' \
          midi-wildmidi.c
      '';

      buildPhase = ''
        gcc -shared -Wl,-soname,"$soname.so" -I. \
          -o "$soname.so" -fpic -m32 -O2 -Wall \
          "$sourceFile" -l"$midiLib"
      '';

      installPhase = ''
        install -vD "$soname.so" "$out/lib/$soname.so"
      '';
    };

    extraAttrs = lib.optionalAttrs (variant == "wildmidiA") {
      soname = "midiA-wildmidi";
      sourceFile = "albion/midiA-wildmidi.c";
      midiLib = "WildMidiA";
    };

  in stdenv.mkDerivation (commonDrvAttrs // extraAttrs);

  udis86 = stdenv.mkDerivation {
    name = "udis86";
    src = "${staticRecompilerSource}/SR/udis86-1.6";
    postPatch = "chmod +x configure mkinstalldirs";
    preInstall = "mkdir -p \"$out/lib\" \"$out/bin\"";
  };

  gameData = stdenv.mkDerivation rec {
    name = "albion-game-data-${version}";
    version = "3";

    src = fetchGog {
      productId = 1436955815;
      downloadName = "${language}1installer1";
      sha256 = ({
        de = "0ylhma70kcj255i03gy5xa3adb8hfw2xpk1m2pp5880aqkmr06k7";
        en = "0x0s2q0x7kjz6qfhb9qs5d959caijiinpc7xv4rx9n7mmb7xlh5m";
      }).${language};
    };

    outputs = [ "out" "dev" ];

    nativeBuildInputs = [ innoextract ];
    phases = [ "unpackPhase" "patchPhase" "installPhase" ];
    unpackCmd = toString [
      "innoextract"
      "--include" "game.gog"
      "--include" "game.ins"
      "--include" "MAIN.EXE"
      "--include" "SETUP.INI"
      "-m" "\"$curSrc\""
    ];
    patchPhase = ''
      sed -i -e '
        s,^SOURCE_PATH=.*,SOURCE_PATH=C:\\,
        s/^\(MODE_[^=]*=\)N$/\1Y/
      ' SETUP.INI
    '';
    installPhase = ''
      ${bchunk}/bin/bchunk game.gog game.ins game_cd
      ${p7zip}/bin/7z x game_cd01.iso ALBION
      mv ALBION "$out"
      install -vD -m 0644 SETUP.INI "$out/setup.ini"
      install -vD -m 0644 MAIN.EXE "$dev/albion_main.exe"
    '';
  };

in buildSandbox (stdenv.mkDerivation {
  name = "albion-${version}";
  inherit version;

  src = staticRecompilerSource;

  patches = [
    ./scons.patch ./xdg-paths.patch ./config.patch ./sdl2.patch
    ./error-log-stderr.patch ./cdpath-is-gamedir.patch
    ./storepaths.patch
  ];

  wildmidi = compileMidiPlugin "wildmidi";
  wildmidiA = compileMidiPlugin "wildmidiA";

  postPatch = ''
    substituteInPlace games/Albion/SR-Main/main.c \
      --subst-var-by GAME_CONFIG_FILE "$out/etc/albion.cfg" \
      --subst-var-by GAME_DATA_PATH ${lib.escapeShellArg gameData.out}

    substituteInPlace games/Albion/SR-Main/virtualfs.c \
      --subst-var-by SETUP_INI_PATH "${gameData.out}/setup.ini"

    substituteInPlace games/Albion/SR-Main/Albion-music-midiplugin.c \
      --replace ./midi-wildmidi.so "$wildmidi/lib/midi-wildmidi.so" \
      --replace ./midiA-wildmidi.so "$wildmidiA/lib/midiA-wildmidi.so" \
  '';

  nativeBuildInputs = [ scons judy python nasm udis86 makeWrapper ];
  buildInputs = [ SDL2 SDL2_mixer ];

  NIX_CFLAGS_COMPILE = "-I${lib.getDev SDL2}/include/SDL2";

  buildPhase = ''
    scons -C SR debug=1

    mkdir tmp
    pushd tmp
    cp ../SR-games/Albion/SR/x86/*.sci .
    ../SR/SR.exe ${gameData.dev}/albion_main.exe Albion-main.asm
    rm *.sci
    python ../SR-games/Albion/SR/compact_source.py
    nasm -felf -dELF -O1 -w+orphan-labels -w-number-overflow \
      -i../SR-games/Albion/SR/x86/ Albion-main_linux.asm 2> a.a || :
    python ../SR-games/Albion/SR/repair_short_jumps.py
    popd

    mv tmp/seg*.inc tmp/Albion-main.asm tmp/Albion-main_linux.asm \
      games/Albion/SR-Main/x86
    rm -r tmp

    scons -C games/Albion/SR-Main debug=1 device=pc-linux sdl2=1
  '';

  installPhase = ''
    install -vD -m 0644 games/Albion/release/linux/Albion.cfg \
      "$out/etc/albion.cfg"
    install -vD games/Albion/SR-Main/SR-Main "$out/bin/albion"

    # XXX: Temporary workaround, because SDL tries to dlopen() libGL.
    wrapProgram "$out/bin/albion" \
      --set SDL_OPENGL_LIBRARY ${lib.escapeShellArg "${libGL}/lib/libGL.so"}
  '';
}) {
  paths.required = [ "$XDG_DATA_HOME/albion" "$XDG_CONFIG_HOME/albion" ];
  paths.runtimeVars = [ "LD_LIBRARY_PATH" ];
}
