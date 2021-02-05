{ stdenv, lib, fetchHumbleBundle, libGL, libpulseaudio, alsaLib, libudev
, writeText
}:

stdenv.mkDerivation rec {
  name = "dott-remastered-${version}";
  version = "1.4.1";

  src = fetchHumbleBundle {
    machineName = "dayofthetentacle_linux_beYLi";
    suffix = "tar.gz";
    md5 = "667b2a8a082702832242321515e55e70";
  };

  unpackCmd = "mkdir \"$name\" && tar xf \"$curSrc\" -C \"$name\"";

  preloader = writeText "dott-preloader.c" ''
    #define _GNU_SOURCE
    #include <dlfcn.h>
    #include <fcntl.h>
    #include <string.h>
    #include <unistd.h>

    static int datadir_size = sizeof(DOTT_DATADIR) - 1;

    ssize_t readlink(const char *path, char *buf, size_t bufsize) {
      static ssize_t (*_readlink) (const char *, char *, size_t) = NULL;
      if (_readlink == NULL) _readlink = dlsym(RTLD_NEXT, "readlink");

      if (strncmp(path, "/proc/self/exe", 15) == 0) {
        ssize_t copylen = datadir_size > bufsize ? bufsize : datadir_size;
        memcpy(buf, DOTT_DATADIR, copylen);
        return copylen;
      } else {
        return _readlink(path, buf, bufsize);
      }
    }

    int SteamAPI_Init(void) {
      return 0;
    }
  '';

  rpath = lib.makeLibraryPath [
    libGL stdenv.cc.cc libpulseaudio alsaLib.out libudev
  ];

  buildPhase = ''
    cc -Werror -Wall -std=gnu11 -shared "$preloader" -o preload.so -fPIC \
      -DDOTT_DATADIR="\"$out/share/dott/DUMMY\""
    patchelf --set-rpath "$rpath" lib/libfmod.so.8
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --remove-needed libsteam_api.so \
      --add-needed "$out/libexec/dott/libdott-preload.so" \
      Dott
    patchelf --set-rpath "$out/libexec/dott:$rpath" Dott
  '';

  installPhase = ''
    install -vsD preload.so "$out/libexec/dott/libdott-preload.so"
    install -vD lib/libfmod.so.8 "$out/libexec/dott/libfmod.so.8"
    install -vD Dott "$out/bin/dott"
    if ldd "$out/bin/dott" | grep -F 'not found'; then exit 1; fi
    install -vD -m 0644 tenta.cle "$out/share/dott/tenta.cle"
  '';

  dontStrip = true;
  dontPatchELF = true;
}
