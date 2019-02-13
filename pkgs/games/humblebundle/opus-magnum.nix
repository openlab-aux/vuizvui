{ stdenv, lib, buildGame, fetchHumbleBundle, makeWrapper, mono
, SDL2, SDL2_image, SDL2_mixer, libvorbis
}:

buildGame rec {
  name = "opus-magnum-${version}";
  version = "20180111";

  src = fetchHumbleBundle {
    machineName = "opus_magnum_4vffp_linux_Z9zWf";
    suffix = "zip";
    md5 = "8bfc49528b69630f8df983fd545518bb";
  };

  arch = if stdenv.system == "x86_64-linux" then "x86_64" else "x86";

  nativeBuildInputs = [ makeWrapper ];

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      SDL2_mixer = "${SDL2_mixer}/lib/libSDL2_mixer.so";
      libvorbisfile-3 = "${libvorbis}/lib/libvorbisfile.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="${target}"!
    }' Lightning.exe.config
  '') dllmap);

  installPhase = ''
    mkdir -p "$out/bin" "$out/share/opus-magnum" "$out/libexec/opus-magnum"
    cp -rvt "$out/share/opus-magnum" Content PackedContent
    cp -rvt "$out/libexec/opus-magnum" Lightning.exe* Ionic.Zip.Reduced.dll

    makeWrapper ${lib.escapeShellArg mono}/bin/mono "$out/bin/opus-magnum" \
      --add-flags "$out/libexec/opus-magnum/Lightning.exe" \
      --run "cd '$out/share/opus-magnum'"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Opus Magnum" "$HOME/Desktop" ];
}
