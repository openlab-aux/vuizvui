{ stdenv, buildGame, fetchGog, writeText, SDL2, libudev }:

buildGame rec {
  name = "freedom-planet-${version}";
  version = "1.21.5";

  src = fetchGog {
    productId = 1207667013;
    downloadName = "en3installer0";
    sha256 = "1rsa2bswzvc4a8crpzhcw3vjan0f9avk7g1gyqibnyppib63i42w";
  };

  binDir = "bin${if stdenv.is64bit then "64" else "32"}";
  buildInputs = [ SDL2 ];
  runtimeDependencies = [ libudev ];

  buildPhase = ''
    cc -Werror -Wall -std=gnu11 -shared "$preloader" -o preload.so -fPIC \
      -DDATA_DIR="\"$out/share/freedom-planet\""
    patchelf \
      --add-needed "$out/libexec/freedom-planet/libpreload.so" \
      "$binDir/Chowdren"
  '';

  preloader = writeText "freedom-planet-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>

    static char *mkDataPath(const char *path)
    {
      size_t pathlen;
      char *buf;

      pathlen = strlen(path);
      buf = malloc(pathlen + sizeof(DATA_DIR));
      if (buf == NULL) return NULL;

      memcpy(buf, DATA_DIR, sizeof(DATA_DIR) - 1);
      memcpy(buf + sizeof(DATA_DIR) - 1, path, pathlen + 1);

      return buf;
    }

    static char *mkSavePath(const char *path)
    {
      char *env;
      char *buf;

      if ((env = getenv("XDG_DATA_HOME")) != NULL) {
        if (asprintf(&buf, "%s/freedom-planet%s", env, path + 1) == -1)
          return NULL;
      } else if ((env = getenv("HOME")) != NULL) {
        if (asprintf(&buf, "%s/.local/share/freedom-planet%s", env,
                     path + 1) == -1)
          return NULL;
      }

      return buf;
    }

    static char *mkCfgPath(const char *path)
    {
      char *env;
      char *buf;

      if ((env = getenv("XDG_CONFIG_HOME")) != NULL) {
        if (asprintf(&buf, "%s/freedom-planet%s", env, path + 1) == -1)
          return NULL;
      } else if ((env = getenv("HOME")) != NULL) {
        if (asprintf(&buf, "%s/.config/freedom-planet%s", env, path + 1) == -1)
          return NULL;
      }

      return buf;
    }

    FILE *fopen(const char *path, const char *mode) {
      FILE *ret;
      char *buf;

      static FILE *(*_fopen) (const char *, const char *) = NULL;
      if (_fopen == NULL) _fopen = dlsym(RTLD_NEXT, "fopen");

      if (strncmp(path, "./Assets.dat", 13) == 0) {
        buf = mkDataPath(path + 1);
      } else if (strncmp(path, "./Data/", 7) == 0) {
        buf = mkDataPath(path + 6);
      } else if (strncmp(path, "./records.dat", 14) == 0 ||
                 strncmp(path, "./file", 6) == 0 ||
                 strncmp(path, "./save", 6) == 0) {
        buf = mkSavePath(path);
      } else if (strncmp(path, "./control_", 10) == 0) {
        buf = mkCfgPath(path);
      } else {
        return _fopen(path, mode);
      }
      if (buf == NULL) return NULL;
      ret = fopen(buf, mode);
      free(buf);
      return ret;
    }
  '';

  installPhase = ''
    install -m 0644 -vD Assets.dat "$out/share/freedom-planet/Assets.dat"
    install -vD "$binDir/Chowdren" "$out/bin/freedom-planet"
    install -vD preload.so "$out/libexec/freedom-planet/libpreload.so"
    cp -rt "$out/share/freedom-planet" Data/BGM Data/font.bmp Data/voices
  '';

  sandbox.paths.required = [
    "$XDG_DATA_HOME/freedom-planet" "$XDG_CONFIG_HOME/freedom-planet"
  ];
}
