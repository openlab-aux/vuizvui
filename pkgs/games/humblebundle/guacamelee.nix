{ stdenv, fetchHumbleBundle, unzip, SDL2, mesa, writeText, makeWrapper }:

stdenv.mkDerivation rec {
  name = "guacamelee-${version}";
  version = "1393037377";

  src = fetchHumbleBundle {
    machineName = "guacamelee_goldedition_linux";
    suffix = "sh";
    md5 = "b06af932c1aaefb8f157c977061388ef";
  };

  unpackCmd = ''
    ${unzip}/bin/unzip "$src" 'data/*' || :
  '';

  preloader = writeText "guacamelee-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>

    int chdir(const char *path) {
      int (*_chdir) (const char *) = dlsym(RTLD_NEXT, "chdir");
      return _chdir(DATA);
    }
  '';

  buildInputs = [ makeWrapper ];

  buildPhase = let
    rpath = stdenv.lib.makeLibraryPath [ SDL2 stdenv.cc.cc mesa ];
    fmodRpath = stdenv.lib.makeLibraryPath [ stdenv.cc.cc ];
  in ''
    gcc -Werror -shared "$preloader" -o preloader.so -ldl \
      -DDATA=\"$out/share/guacamelee\"

    for i in libfmodevent-4.44.27.so libfmodex-4.44.27.so; do
      patchelf --set-rpath "${fmodRpath}:$out/libexec/guacamelee" \
        "x86/lib32/$i"
    done
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}:$out/libexec/guacamelee" x86/game-bin
  '';

  installPhase = ''
    install -vD x86/game-bin "$out/libexec/guacamelee/guacamelee"
    install -vD preloader.so "$out/libexec/guacamelee/preloader.so"

    makeWrapper "$out/libexec/guacamelee/guacamelee" "$out/bin/guacamelee" \
      --set LD_PRELOAD "$out/libexec/guacamelee/preloader.so"

    for i in libfmodevent-4.44.27.so libfmodex-4.44.27.so; do
      install -vD "x86/lib32/$i" "$out/libexec/guacamelee/$i"
    done

    mkdir -p "$out/share"
    cp -vRd noarch "$out/share/guacamelee"
  '';

  dontStrip = true;
}
