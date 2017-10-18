{ stdenv, fetchHumbleBundle, unzip, fetchurl, writeText, SDL2, mesa, xorg
, makeDesktopItem
}:

let
  binaryDeps = {
    starbound.deps = [
      SDL2 mesa xorg.libX11 xorg.libICE xorg.libSM xorg.libXext
    ];
    starbound.needsBootconfig = true;

    starbound_server.name = "starbound-server";
    starbound_server.needsBootconfig = true;

    asset_packer.name = "starbound-asset-packer";
    asset_unpacker.name = "starbound-asset-unpacker";

    dump_versioned_json.name = "starbound-dump-versioned-json";
    make_versioned_json.name = "starbound-make-versioned-json";

    planet_mapgen.name = "starbound-planet-mapgen";
  };

  desktopItem = makeDesktopItem {
    name = "starbound";
    exec = "starbound";
    icon = fetchurl {
      url = "http://i1305.photobucket.com/albums/s544/ClockworkBarber/"
          + "logo_zps64c4860d.png";
      sha256 = "11fiiy0vcxzix1j81732cjh16wi48k4vag040vmbhad50ps3mg0q";
    };
    comment = "An extraterrestrial sandbox adventure game";
    desktopName = "Starbound";
    genericName = "starbound";
    categories = "Game;";
  };

  patchBinary = bin: attrs: ''
    mkdir -p "patched/$(dirname "${bin}")"
    cp -t "patched/$(dirname "${bin}")" "linux/${bin}"
    chmod +x "patched/$(basename "${bin}")"
    ${stdenv.lib.optionalString (attrs.needsBootconfig or false) ''
      for offset in $(
        grep -abz '^sbinit\.config''$' "patched/$(basename "${bin}")" \
          | cut -d: -f1 -z | xargs -0
      ); do
        for i in $(seq 13); do printf '\x07'; done \
          | dd of="patched/$(basename "${bin}")" \
               obs=1 seek="$offset" \
               count=13 conv=notrunc
      done
      patchelf \
        --add-needed "$lib/lib/libstarbound-preload.so" \
        "patched/$(basename "${bin}")"
    ''}
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${stdenv.lib.makeLibraryPath (attrs.deps or [])}" \
      "patched/$(basename "${bin}")"
    if ldd "patched/$(basename "${bin}")" | grep -F 'not found' \
       | grep -v 'libstarbound-preload\.so\|libsteam_api\.so'; then
      exit 1;
    fi
  '';

  preloaderSource = writeText "starbound-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <fcntl.h>
    #include <malloc.h>
    #include <stdarg.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <sys/stat.h>
    #include <sys/wait.h>
    #include <unistd.h>

    #define MAGIC "\x07\x07\x07\x07\x07\x07\x07\x07\x07\x07\x07\x07\x07"

    static char *getXdgDataHome(void) {
      int envlen;
      char *result;
      const char *env;

      if ((env = getenv("XDG_DATA_HOME")) != NULL)
        return strdup(env);
      if ((env = getenv("HOME")) == NULL)
        return NULL;

      envlen = strlen(env);
      if ((result = malloc(envlen + 14)) == NULL)
        return NULL;
      strncpy(result, env, envlen);
      strncpy(result + envlen, "/.local/share", 14);
      return result;
    }

    static char *mkJsonString(const char *str) {
      char *result, *out;

      if ((result = malloc(strlen(str) * 6 + 3)) == NULL)
        return NULL;

      out = result;
      *out++ = '"';

      for (; *str != '\0'; str++) {
        switch (*str) {
          case '"':  *out++ = '\\'; *out++ = '"'; break;
          case '\\': *out++ = '\\'; *out++ = '\\'; break;
          case '\b': *out++ = '\\'; *out++ = 'b'; break;
          case '\f': *out++ = '\\'; *out++ = 'f'; break;
          case '\n': *out++ = '\\'; *out++ = 'n'; break;
          case '\r': *out++ = '\\'; *out++ = 'r'; break;
          case '\t': *out++ = '\\'; *out++ = 't'; break;
          default:
            if (*str >= 0 && *str <= 31) {
              *out++ = '\\';
              *out++ = 'u';
              snprintf(out, 4, "%04x", *str);
              out += 4;
            } else {
              *out++ = *str;
            }
            break;
        }
      }

      *out++ = '"';
      *out = 0;

      return result;
    }

    static char *mkDataDir(const char *dataHome, const char *append) {
      char *buf, *out;
      int homeLen = strlen(dataHome);
      int appendLen = strlen(append);

      if ((buf = malloc(homeLen + appendLen + 2)) == NULL)
        return NULL;

      snprintf(buf, homeLen + appendLen + 2, "%s/%s", dataHome, append);

      out = mkJsonString(buf);
      free(buf);

      return out;
    }

    static int makedirs(const char *path) {
      char *buf, *p;

      if (strlen(path) == 0)
        return 1;

      if ((buf = strdup(path)) == NULL)
        return 1;

      for (p = buf + 1; *p != '\0'; p++) {
        if (*p != '/') continue;
        *p = '\0';
        mkdir(buf, 0777);
        *p = '/';
      }

      free(buf);
      return 0;
    }

    static int writeSBInit(FILE *sbinit) {
      char *buf, *dataHome;
      int homeLen, ret;

      if ((dataHome = getXdgDataHome()) == NULL)
        return 1;

      homeLen = strlen(dataHome);
      if ((buf = malloc(homeLen + 12)) == NULL)
        goto errout;
      strncpy(buf, dataHome, homeLen);
      strncpy(buf + homeLen, "/starbound/", 12);
      ret = makedirs(buf);
      free(buf);
      if (ret != 0) goto errout;

      fputs("{\"assetDirectories\":[", sbinit);

      if ((buf = mkJsonString(STARBOUND_ASSET_DIR)) == NULL)
        goto errout;
      fputs(buf, sbinit);
      free(buf);

      fputc(',', sbinit);

      if ((buf = mkDataDir(dataHome, "starbound/mods/")) == NULL)
        goto errout;
      fputs(buf, sbinit);
      free(buf);

      fputs("],\"storageDirectory\":", sbinit);

      if ((buf = mkDataDir(dataHome, "starbound/")) == NULL)
        goto errout;
      fputs(buf, sbinit);
      free(buf);

      fputs("}", sbinit);
      free(dataHome);
      return 0;
    errout:
      free(dataHome);
      return 1;
    }

    static FILE *fakeSBInitHandle = NULL;

    static int fakeSBInit(void) {
      fakeSBInitHandle = tmpfile();
      if (writeSBInit(fakeSBInitHandle) != 0) {
        fclose(fakeSBInitHandle);
        fakeSBInitHandle = NULL;
        return -1;
      }
      rewind(fakeSBInitHandle);
      return fileno(fakeSBInitHandle);
    }

    int open(const char *path, int flags, ...) {
      va_list ap;
      mode_t mode;
      static int (*_open) (const char *, int, mode_t) = NULL;

      if (_open == NULL)
        _open = dlsym(RTLD_NEXT, "open");

      va_start(ap, flags);
      mode = va_arg(ap, mode_t);
      va_end(ap);

      if (strncmp(path, MAGIC, 14) != 0)
        return _open(path, flags, mode);

      return fakeSBInit();
    }

    int close(int fd) {
      int ret;
      static int (*_close) (int) = NULL;

      if (_close == NULL)
        _close = dlsym(RTLD_NEXT, "close");

      if (fakeSBInitHandle != NULL) {
        ret = fclose(fakeSBInitHandle);
        fakeSBInitHandle = NULL;
      } else {
        ret = _close(fd);
      }

      return ret;
    }

    int SteamAPI_Init(void) {
      return 0;
    }
  '';

in stdenv.mkDerivation rec {
  name = "starbound-${version}";
  version = "1.3.3";

  src = fetchHumbleBundle {
    name = "starbound-linux-${version}.zip";
    machineName = "starbound_linux";
    md5 = "91decc3a8fa9cd0be08952422f5adb39";
  };

  outputs = [ "out" "lib" "assets" ];

  nativeBuildInputs = [ unzip ];

  buildPhase = with stdenv.lib; ''
    cc -Werror -shared "${preloaderSource}" -o preload.so -ldl -fPIC \
      -DSTARBOUND_ASSET_DIR="\"$assets\""
    ${concatStrings (mapAttrsToList patchBinary binaryDeps)}
    patchelf --remove-needed libsteam_api.so patched/starbound
  '';

  doCheck = true;

  checkPhase = ''
    checkFailed=
    for i in linux/*; do
      [ -f "$i" ] || continue

      case "$(basename "$i")" in
        sbinit.config) continue;;
        *.s[ho]) continue;;
      esac

      [ ! -e "patched/$(basename "$i")" ] || continue

      echo "Found missing binary $i from the upstream tree."
      checkFailed=1
    done

    [ -z "$checkFailed" ]
  '';

  installPhase = ''
    install -vsD preload.so "$lib/lib/libstarbound-preload.so"

    ${stdenv.lib.concatStrings (stdenv.lib.mapAttrsToList (bin: attrs: let
      basename = builtins.baseNameOf bin;
    in ''
      install -vD "patched/${basename}" "$out/bin/${attrs.name or basename}"
    '') binaryDeps)}

    install -m 0644 -vD "${desktopItem}/share/applications/starbound.desktop" \
      "$out/share/applications/starbound.desktop"

    cp -vr assets "$assets"
  '';

  dontStrip = true;
}
