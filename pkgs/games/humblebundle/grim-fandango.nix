{ stdenv, fetchHumbleBundle, mesa, libpulseaudio, alsaLib, SDL2, writeText
, xorg
}:

stdenv.mkDerivation rec {
  name = "grim-fandango-remastered-${version}";
  version = "1.4.0";

  src = fetchHumbleBundle {
    machineName = "grimfandango_linux";
    suffix = "tar.gz";
    md5 = "52e0590850102a1ae0db907bef413e57";
  };

  preloader = writeText "grim-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <dirent.h>
    #include <fcntl.h>
    #include <unistd.h>
    #include <stdlib.h>
    #include <stdio.h>
    #include <string.h>
    #include <sys/stat.h>
    #include <sys/types.h>

    int chdir(const char *path) {
      static int (*_chdir) (const char *) = NULL;
      if (_chdir == NULL) {
        _chdir = dlsym(RTLD_NEXT, "chdir");
        return _chdir(GRIM_DATADIR);
      }
      return _chdir(path);
    }

    #define CONCAT_ENV(path) \
      if (asprintf(&result, "%s/%s", env, path) == -1) { \
        perror("asprintf"); \
        exit(1); \
      }

    static char *getSaveDir(void) {
      const char *env;
      static char *result = NULL;

      if (result == NULL) {
        if ((env = getenv("XDG_DATA_HOME")) != NULL) {
          CONCAT_ENV("grim-fandango");
        } else if ((env = getenv("HOME")) != NULL) {
          CONCAT_ENV(".local/share/grim-fandango");
        } else {
          fputs("Unable to determine XDG_DATA_HOME or HOME.\n", stderr);
          exit(1);
        }
      }

      return result;
    }

    static void makedirs(char *path)
    {
      int pathlen = strlen(path);

      static int (*_mkdir) (const char *, mode_t) = NULL;
      if (_mkdir == NULL) _mkdir = dlsym(RTLD_NEXT, "mkdir");

      for (int i = 1; i < pathlen; ++i) {
        if (path[i] == '/') {
          path[i] = '\0';
          _mkdir(path, 0777);
          path[i] = '/';
        }
      }
    }

    static char *mkSavePath(const char *path)
    {
      int savelen, pathlen;
      char *buf, *savedir;

      savedir = getSaveDir();
      savelen = strlen(savedir);
      pathlen = strlen(path);
      buf = malloc(savelen + pathlen + 1);
      strncpy(buf, savedir, savelen);
      strncpy(buf + savelen, path, pathlen + 1);

      return buf;
    }

    int mkdir(const char *pathname, mode_t mode) {
      return 0;
    }

    FILE *fopen64(const char *path, const char *mode) {
      FILE *fp;
      char *buf;

      static FILE *(*_fopen) (const char *, const char *) = NULL;
      if (_fopen == NULL) _fopen = dlsym(RTLD_NEXT, "fopen64");

      if (strncmp(path, "./Saves/", 8) == 0) {
        path += 7;
        buf = mkSavePath(path);
        if ((fp = _fopen(buf, mode)) == NULL) {
          makedirs(buf);
          fp = _fopen(buf, mode);
        }
        free(buf);
        return fp;
      }

      return _fopen(path, mode);
    }

    DIR *opendir(const char *name) {
      DIR *dp;
      char *buf;

      static DIR *(*_opendir) (const char *) = NULL;
      if (_opendir == NULL) _opendir = dlsym(RTLD_NEXT, "opendir");

      if (strncmp(name, "./Saves/", 8) == 0) {
        name += 7;
        buf = mkSavePath(name);
        if ((dp = _opendir(buf)) == NULL) {
          makedirs(buf);
          dp = _opendir(buf);
        }
        free(buf);
        return dp;
      }

      return _opendir(name);
    }
  '';

  rpath = stdenv.lib.makeLibraryPath [
    mesa stdenv.cc.cc libpulseaudio alsaLib.out SDL2 xorg.libX11
  ];

  buildPhase = ''
    cc -Werror -Wall -std=gnu11 -shared "$preloader" -o preload.so -fPIC \
      -DGRIM_DATADIR="\"$out/share/grim-fandango\""
    patchelf --set-rpath "$rpath" bin/libchore.so
    patchelf --set-rpath "$rpath" bin/libLua.so
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --add-needed "$out/libexec/grim-fandango/libgrim-preload.so" \
      bin/GrimFandango
    patchelf --replace-needed libSDL2-2.0.so.1 libSDL2.so bin/GrimFandango
    patchelf --set-rpath "$out/libexec/grim-fandango:$rpath" bin/GrimFandango
  '';

  installPhase = ''
    install -vsD preload.so "$out/libexec/grim-fandango/libgrim-preload.so"
    install -vD bin/libchore.so "$out/libexec/grim-fandango/libchore.so"
    install -vD bin/libLua.so "$out/libexec/grim-fandango/libLua.so"
    install -vD bin/GrimFandango "$out/bin/grim-fandango"
    if ldd "$out/bin/grim-fandango" | grep -F 'not found'; then exit 1; fi

    mkdir -p "$out/share/grim-fandango"
    find bin -maxdepth 1 -mindepth 1 \
      \( -path bin/i386 \
      -o -path bin/amd64 \
      -o -path bin/common-licenses \
      -o -path bin/scripts \
      -o -path bin/GrimFandango \
      -o -name '*.so' \
      -o -name '*.so.*' \
      -o -name '*.txt' \
      \) -prune -o -print | xargs cp -vrt "$out/share/grim-fandango"
  '';

  dontStrip = true;
  dontPatchELF = true;
}
