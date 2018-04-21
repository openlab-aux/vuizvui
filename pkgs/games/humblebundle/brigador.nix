{ stdenv, fetchurl, makeWrapper, fetchHumbleBundle, writeText
, SDL2, libGL, glew, freeimage
}:

let
  oldGLEW = glew.overrideDerivation (stdenv.lib.const rec {
    name = "glew-1.12.0";
    src = fetchurl {
      url = "mirror://sourceforge/glew/${name}.tgz";
      sha256 = "1gz4917k9iyv3s8k0fxylzrwdnlf7dcszlsfzbkl7d1490zi0n5g";
    };
  });

in stdenv.mkDerivation {
  name = "brigador-1.0";

  src = fetchHumbleBundle {
    machineName = "brigador_linux";
    suffix = "tar";
    md5 = "61af4a5f037b85bf6acc5ca76d295d09";
  };

  preloader = writeText "brigador-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <fcntl.h>
    #include <stdarg.h>
    #include <stdio.h>
    #include <string.h>
    #include <sys/stat.h>

    #define MANGLE_PATH(call, ...) \
      if (strncmp(path, "assets.pack", 12) == 0 || \
          strncmp(path, "assets/",     7)  == 0 || \
          strncmp(path, "fonts/",      6)  == 0 || \
          strncmp(path, "shaders/",    8)  == 0 || \
          strncmp(path, "sounds/",     7)  == 0) { \
        char buf[1024]; \
        snprintf(buf, sizeof(buf), "%s/%s", LIBEXEC_PATH, path); \
        return call(buf, __VA_ARGS__); \
      }

    ${stdenv.lib.concatMapStrings (fun: ''
      FILE *${fun}(const char *path, const char *mode) {
        static FILE *(*_${fun}) (const char *, const char *) = NULL;
        if (_${fun} == NULL) _${fun} = dlsym(RTLD_NEXT, "${fun}");
        MANGLE_PATH(_${fun}, mode);
        return _${fun}(path, mode);
      }
    '') [ "fopen" "fopen64" ]}

    int open(const char *path, int flags, ...) {
      va_list ap;
      mode_t mode;
      static int (*_open) (const char *, int, mode_t) = NULL;
      if (_open == NULL) _open = dlsym(RTLD_NEXT, "open");
      va_start(ap, flags);
      mode = va_arg(ap, mode_t);
      va_end(ap);
      MANGLE_PATH(_open, (flags & ~O_RDWR) | O_RDONLY, mode);
      return _open(path, flags, mode);
    }
  '';

  patchPhase = let
    fmodRpath = stdenv.lib.makeLibraryPath [ "$out" stdenv.cc.cc ];
    rpath = stdenv.lib.makeLibraryPath [ "$out" SDL2 libGL oldGLEW freeimage ];
  in ''
    for fmod in lib/libfmod*.so*; do
      patchelf --set-rpath "${fmodRpath}" "$fmod"
    done

    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" brigador
  '';

  buildPhase = ''
    cc -Werror -shared "$preloader" -o preloader.so -ldl -fPIC \
      -DLIBEXEC_PATH=\"$out/libexec/brigador\"
  '';

  buildInputs = [ makeWrapper ];

  installPhase = ''
    for fmod in lib/libfmod*.so*; do
      install -vD "$fmod" "$out/lib/$(basename "$fmod")"
    done

    install -vD brigador "$out/libexec/brigador/brigador"
    install -vD preloader.so "$out/libexec/brigador/preloader.so"
    install -vD -m 0644 assets.pack "$out/libexec/brigador/assets.pack"
    cp -rt "$out/libexec/brigador" assets fonts shaders sounds

    makeWrapper "$out/libexec/brigador/brigador" "$out/bin/brigador" \
      --set LD_PRELOAD "$out/libexec/brigador/preloader.so" \
      --run 'XDG_DATA_HOME="''${XDG_DATA_HOME:-$HOME/.local/share}"' \
      --run 'mkdir -p "$XDG_DATA_HOME/brigador"' \
      --run 'cd "$XDG_DATA_HOME/brigador"'
  '';

  dontStrip = true;
}
