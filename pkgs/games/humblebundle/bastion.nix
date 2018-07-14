{ lib, stdenv, buildGame, fetchHumbleBundle, makeWrapper, unzip, imagemagick
, mono, SDL2, SDL2_image, openal, libvorbis, libGL
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

  nativeBuildInputs = [ makeWrapper imagemagick ];

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

    # FMOD Ex tries to run /bin/sh -c 'pulseaudio --check > /dev/null 2>&1', so
    # we need to prevent this by replacing the system() call with a successful
    # return value (0). If someone doesn't have or want to have PulseAudio,
    # FMOD Ex still falls back to ALSA if it can't load libpulse-simple.so.
    if [ "$libDir" = lib64 ]; then
      addr="$(objdump -d lib64/libfmodex.so | sed -n -e '
        /callq.*system@plt/s/^ *\([^:]\+\).*/\1/p
      ')"

      # This is quite easy, just replace the system() call with XOR EAX so we
      # get a return value of 0 and pad the rest with NOP.
      offset=$(("0x$addr"))
      ( printf '\x31\xc0'     # XOR the EAX register
        printf '\x90\x90\x90' # Fill with NOPs
      ) | dd of="$libDir/libfmodex.so" obs=1 seek=$offset count=5 conv=notrunc
    else
      relocSystem="$(readelf -r lib/libfmodex.so | sed -n -e '
        /system@/s/^0*\([^ ]\+\).*/\1/p
      ')"
      addr="$(objdump -d lib/libfmodex.so | sed -n -e '
        /call *'"$relocSystem"' /s/^ *\([^:]\+\).*/\1/p
      ')"

      # For the 32 bit library it's not so easy as the 4 bytes coming after the
      # CALL opcode will be replaced by the dynamic linker, so we just XOR the
      # EAX register with the relocation address and replace the TEST opcode
      # afterwards.
      offset=$(("0x$addr"))
      ( printf '\x35\xfc\xff\xff\xff' # XOR EAX with the relocation address
        printf '\x39\xc0'             # CMP EAX, EAX
      ) | dd of="$libDir/libfmodex.so" obs=1 seek=$offset conv=notrunc
    fi

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

  # Add all of the libraries in $runtimeDependencies to the FMOD Ex library,
  # because it tries to dlopen() libpulse-simple and libasound.so and we don't
  # have a main ELF binary where we could add that search path.
  postFixup = ''
    rpath="$(patchelf --print-rpath "$out/libexec/bastion/libfmodex.so")"
    for dep in $runtimeDependencies; do
      rpath="$rpath${rpath:+:}$dep/lib"
    done
    patchelf --set-rpath "$rpath" "$out/libexec/bastion/libfmodex.so"
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Bastion" ];
}
