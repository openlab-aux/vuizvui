{ buildGame, fetchHumbleBundle, makeWrapper, runCommandCC, writeText
, coreutils, openal, libvorbis, libGLU, SDL2, freetype, alsa-lib
}:

buildGame rec {
  name = "trine2-${version}";
  version = "2.01";

  src = fetchHumbleBundle {
    machineName = "trine2complete_linux";
    suffix = "zip";
    md5 = "82049b65c1bce6841335935bc05139c8";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ openal libvorbis libGLU freetype alsa-lib ];

  patchPhase = ''
    patchelf --replace-needed libSDL-1.3.so.0 libSDL.so \
      bin/trine2_linux_32bit
    patchelf --replace-needed libPhysXLoader.so.1 libPhysXLoader.so \
      bin/trine2_linux_32bit
  '';

  getResolutionArgs = runCommandCC "get-resolution-args" {
    buildInputs = [ SDL2 ];
    src = writeText "get-resolution-args.c" ''
      #include <SDL.h>

      int main(void)
      {
        int width = 0, height = 0;
        SDL_DisplayMode current;

        SDL_Init(SDL_INIT_VIDEO);

        int displays = SDL_GetNumVideoDisplays();

        for (int i = 0; i < displays; ++i) {
          if (SDL_GetCurrentDisplayMode(i, &current) != 0)
            goto err;

          if (current.w * current.h > width * height) {
            width = current.w;
            height = current.h;
          }
        }

        if (width == 0 && height == 0)
          goto err;

        SDL_Quit();
        printf("-RenderingModule:DetectedFullscreenWidth=%d\n", width);
        printf("-RenderingModule:DetectedFullscreenHeight=%d\n", height);
        return EXIT_SUCCESS;

      err:
        fputs("Unable to get current display mode.\n", stderr);
        SDL_Quit();
        return EXIT_FAILURE;
      }
    '';
  } "gcc -Wall $(sdl2-config --cflags --libs) -o \"$out\" \"$src\"";

  installPhase = ''
    for name in Cg CgGL PhysXCooking PhysXCore PhysXLoader; do
      install -vD "lib/lib32/lib$name.so" "$out/libexec/trine2/lib$name.so"
    done

    install -vD lib/lib32/libSDL-1.3.so.0 "$out/libexec/trine2/libSDL.so"

    mkdir -p "$out/share/trine2"
    cp -rvt "$out/share/trine2" *.fbq trine2.png data

    install -vD bin/trine2_linux_32bit "$out/libexec/trine2/trine2"

    rtDataPath="\''${XDG_DATA_HOME:-\$HOME/.local/share}"
    makeWrapper "$out/libexec/trine2/trine2" "$out/bin/trine2" \
      --run "cd '$out/share/trine2'" \
      --run '${coreutils}/bin/ln -s "'"$rtDataPath"'" "$HOME/.frozenbyte"' \
      --prefix LD_LIBRARY_PATH : "$out/libexec/trine2" \
      --add-flags "\$($getResolutionArgs)"

    mkdir -p "$out/share/applications"
    cat > "$out/share/applications/trine2.desktop" <<EOF
    [Desktop Entry]
    Name=Trine 2
    Type=Application
    Version=1.1
    Exec=$out/bin/trine2
    Icon=$out/share/trine2/trine2.png
    Categories=Game
    EOF
  '';

  sandbox.paths.required = [ "$XDG_DATA_HOME/Trine2" ];
}
