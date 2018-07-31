{ lib, stdenv, buildGame, fixFmodHook, fetchHumbleBundle, makeWrapper
, unzip, imagemagick, mono, SDL2, SDL2_image, openal, libvorbis, libGL
}:

buildGame rec {
  name = "bastion-${version}";
  version = "20161016";

  src = fetchHumbleBundle {
    name = "bastion-20161016.bin";
    machineName = "bastion_linux";
    downloadName = "Download";
    md5 = "19fea173ff2da0f990f60bd5e7c3b237";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/*' || :";

  nativeBuildInputs = [ makeWrapper imagemagick fixFmodHook ];

  libDir =
    if stdenv.system == "i686-linux" then "lib"
    else if stdenv.system == "x86_64-linux" then "lib64"
    else throw "Unsupported architecture ${stdenv.system}.";

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      soft_oal = "${openal}/lib/libopenal.so";
      libvorbisfile = "${libvorbis}/lib/libvorbisfile.so";
      MojoShader = "$out/libexec/bastion/libmojoshader.so";
    };
  in lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="'"${target}"'"!
    }' FNA.dll.config
  '') dllmap) + ''
    # This is not needed, so let's remove it.
    sed -i -e '/libtheoraplay/d' FNA.dll.config

    # We don't want any Steam libraries, that's why we bought the non-DRM
    # version, right?
    cc -Wall -std=c11 -shared -xc - -o "$libDir/libSteamStub.so" -fPIC <<EOF
    #include <stddef.h>
    int SteamAPI_Init(void) { return 0; }
    void SteamAPI_Shutdown(void) {}
    void SteamAPI_RunCallbacks() {}
    /* All the symbols for the global accessors for Steamworks C++ APIs */
    ${lib.concatMapStrings (sym: ''
      void *Steam${sym}(void) { return NULL; }
    '') [
      "Client" "User" "Friends" "Utils" "Matchmaking" "UserStats" "Apps"
      "Networking" "MatchmakingServers" "RemoteStorage" "Screenshots" "HTTP"
      "UnifiedMessages" "Controller" "UGC" "AppList" "Music" "MusicRemote"
      "HTMLSurface" "Inventory" "Video"
    ]}
    EOF
    patchelf --remove-needed libsteam_api.so "$libDir/libSteamWrapper.so"
    patchelf --add-needed libSteamStub.so "$libDir/libSteamWrapper.so"

    # For the Desktop icon
    convert Bastion.bmp Bastion.png
  '';

  installPhase = ''
    for lib in fmodex mojoshader SteamWrapper SteamStub; do
      install -vD "$libDir/lib$lib.so" "$out/libexec/bastion/lib$lib.so"
    done

    cp -rvt "$out/libexec/bastion" Bastion.exe FNA.dll* \
      FMOD.dll Lidgren.Network.dll MonoGame.Framework.Net.dll

    mkdir -p "$out/share"
    cp -rv Content "$out/share/bastion"
    cp -vt "$out/libexec/bastion" Bastion.bmp
    ln -s "$out/share/bastion" "$out/libexec/bastion/Content"

    makeWrapper ${lib.escapeShellArg mono}/bin/mono "$out/bin/bastion" \
      --add-flags "$out/libexec/bastion/Bastion.exe" \
      --set SDL_OPENGL_LIBRARY ${lib.escapeShellArg "${libGL}/lib/libGL.so"} \
      --run "cd '$out/libexec/bastion'"

    install -vD -m 0644 Bastion.png "$out/share/icons/bastion.png"

    mkdir -p "$out/share/applications"
    cat > "$out/share/applications/bastion.desktop" <<EOF
    [Desktop Entry]
    Name=Bastion
    Type=Application
    Version=1.1
    Exec=$out/bin/bastion
    Icon=$out/share/icons/bastion.png
    Categories=Game
    EOF
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Bastion" ];
}
