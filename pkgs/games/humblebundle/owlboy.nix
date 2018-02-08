{ stdenv, lib, buildGame, fetchHumbleBundle, unzip, makeWrapper, mono50
, SDL2, SDL2_image, openal, libvorbis
, writeText
}:

buildGame rec {
  name = "owlboy-${version}";
  version = "20171229";

  src = fetchHumbleBundle {
    suffix = "bin";
    machineName = "owlboy_linux";
    md5 = "c2e99502013c7d2529bc2aefb6416dcf";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/*' || :";

  nativeBuildInputs = [ makeWrapper ];

  libdir = if stdenv.system == "x86_64-linux" then "lib64" else "lib";

  prePatch = ''
    find -type f -name '*.ini' -exec sed -i -e 's/${"\r"}$//' {} +
  '';

  buildPhase = let
    dllmap = {
      SDL2 = "${SDL2}/lib/libSDL2.so";
      SDL2_image = "${SDL2_image}/lib/libSDL2_image.so";
      soft_oal = "${openal}/lib/libopenal.so";
      libvorbisfile-3 = "${libvorbis}/lib/libvorbisfile.so";
      MojoShader = "$out/lib/owlboy/libmojoshader.so";
    };
  in ''
    cc -Werror -shared "$preloader" -o preloader.so -ldl -fPIC \
      -DSTOREPATH=\"$out\"
  '' + lib.concatStrings (lib.mapAttrsToList (dll: target: ''
    sed -i -e '/<dllmap.*dll="${dll}\.dll".*os="linux"/ {
      s!target="[^"]*"!target="'"${target}"'"!
    }' FNA.dll.config
  '') dllmap);

  # The game tries to open data files in read-write mode, so use LD_PRELOAD to
  # avoid this whenever a store path is involved.
  preloader = writeText "owlboy-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <fcntl.h>
    #include <stdarg.h>
    #include <stdio.h>
    #include <stdbool.h>
    #include <string.h>
    #include <sys/stat.h>

    static bool isDataPath(const char *path) {
      return strncmp(path, "content/", 8) == 0
          || strncmp(path, STOREPATH, sizeof(STOREPATH) - 1) == 0;
    }

    int access(const char *path, int mode) {
      static int (*_access) (const char *, int) = NULL;
      if (_access == NULL) _access = dlsym(RTLD_NEXT, "access");
      if (isDataPath(path) && mode & W_OK)
        mode = mode & ~W_OK | R_OK;
      return _access(path, mode);
    }

    int open(const char *path, int flags, ...) {
      va_list ap;
      mode_t mode;
      static int (*_open) (const char *, int, mode_t) = NULL;
      if (_open == NULL) _open = dlsym(RTLD_NEXT, "open");
      va_start(ap, flags);
      mode = va_arg(ap, mode_t);
      va_end(ap);
      if (isDataPath(path) && flags & O_RDWR)
        flags = flags & ~O_RDWR | O_RDONLY;
      return _open(path, flags, mode);
    }
  '';

  installPhase = ''
    mkdir -p "$out/bin" "$out/share" "$out/libexec/owlboy"
    install -vD preloader.so "$out/lib/owlboy/preloader.so"
    cp -rv content "$out/share/owlboy"
    cp -rv monoconfig "$out/libexec/owlboy/Owlboy.exe.config"
    cp -rvt "$out/libexec/owlboy" Owlboy.exe \
      FNA.dll* GamedevUtility.dll MoonSharp.Interpreter.dll TimSort.dll
    cp -rvt "$out/lib/owlboy" "$libdir/libmojoshader.so"
    ln -vs "$out/share/owlboy" "$out/libexec/owlboy/content"

    makeWrapper ${lib.escapeShellArg mono50}/bin/mono \
      "$out/bin/owlboy" \
      --set LD_PRELOAD "$out/lib/owlboy/preloader.so" \
      --add-flags "$out/libexec/owlboy/Owlboy.exe" \
      --run "cd '$out/libexec/owlboy'"
  '';

  sandbox.paths.required = [
    "$XDG_DATA_HOME/Owlboy" "$XDG_CONFIG_HOME/Owlboy"
  ];
}
