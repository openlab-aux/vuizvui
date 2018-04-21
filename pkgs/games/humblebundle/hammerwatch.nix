{ stdenv, fetchHumbleBundle, makeWrapper, unzip, mono, SDL2, libGL, openal
, pulseaudio
}:

# FIXME: Dosn't support the XDG Base Directory Specification,
#        so enforce it using LD_PRELOAD maybe?

stdenv.mkDerivation rec {
  name = "hammerwatch-${version}";
  version = "1.3";

  src = fetchHumbleBundle {
    machineName = "hammerwatch_linux";
    suffix = "zip";
    md5 = "7cd77e4395f394c3062322c96e418732";
  };

  buildInputs = [ unzip makeWrapper ];

  installPhase = let
    rpath = stdenv.lib.makeLibraryPath [ SDL2 libGL openal pulseaudio ];
    monoNoLLVM = mono.override { withLLVM = false; };
  in ''
    mkdir -p "$out/lib"
    cp -rt "$out/lib" SDL2-CS.dll SDL2-CS.dll.config \
      TiltedEngine.dll Lidgren.Network.dll FarseerPhysicsOTK.dll \
      ICSharpCode.SharpZipLib.dll SteamworksManaged.dll NVorbis.dll

    libexec="$out/libexec/hammerwatch"
    install -vD Hammerwatch.exe "$libexec/hammerwatch.exe"
    cp -rt "$libexec" assets.bin editor levels

    makeWrapper "${monoNoLLVM}/bin/mono" "$out/bin/hammerwatch" \
      --add-flags "$libexec/hammerwatch.exe" \
      --set MONO_PATH "$out/lib" \
      --set LD_LIBRARY_PATH "${rpath}"
  '';

  dontStrip = true;
}
