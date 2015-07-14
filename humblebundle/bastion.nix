{ stdenv, fetchHumbleBundle, lzma, xorg, libpulseaudio}:

let
  arch = {
    "i686-linux" = "x86";
    "x86_64-linux" = "x86_64";
  }.${stdenv.system};
in stdenv.mkDerivation rec {
  name = "bastion-1.4";

  src = fetchHumbleBundle {
    name = "Bastion-HIB-2012-06-20.sh";
    machineName = "bastion_linux";
    downloadName = ".sh";
    md5 = "aa6ccaead3b4b8a5fbd156f4019e8c8b";
  };

  dontStrip = true;
  phases = ["installPhase"];

  installPhase = let
    rpath = stdenv.lib.makeLibraryPath [
      "$dest"
      xorg.libX11
      xorg.libXi
      stdenv.cc.cc
      libpulseaudio
    ];
   in ''
    dest="$out/opt/games/bastion"
    libdir="$dest/lib" #${stdenv.lib.optionalString (arch=="x86_64") "64"}"

    # Unpack binaries and data into $dest
    mkdir -p "$dest"
    sh "$src" --tar xf ./instarchive_all -O           | ${lzma}/bin/lzcat | tar x -C "$dest"
    sh "$src" --tar xf ./instarchive_linux_${arch} -O | ${lzma}/bin/lzcat | tar x -C "$dest"

    # Ensure that $dest is a valid library path.
    mv $dest/lib64 $libdir || true

    # Patch heavily :-)
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" "$dest/Bastion.bin.${arch}"
    patchelf --set-rpath "${rpath}" "$libdir/libmono-2.0.so.1"
    patchelf --set-rpath "${rpath}" "$libdir/libfmodex.so"
    patchelf --set-rpath "${rpath}" "$libdir/libSDL-1.2.so.0"

    # Fixup permissions, just to be sure.
    find "$dest" -type f -exec chmod 644 "{}" +
    chmod 755 "$dest/Bastion.bin.${arch}"

    # Taken from ArchLinux; might be useful to actually implement
    #install -Dm644 "''${pkgname}".desktop "''${pkgdir}"/usr/share/applications/"''${pkgname}".desktop
    #install -Dm755 "mesa''${pkgname}" "''${pkgdir}"/usr/bin/"''${pkgname}"mesa
    #install -Dm644 "''${pkgdir}"/opt/games/Bastion/Bastion.png "''${pkgdir}"/usr/share/icons/"''${pkgname}".png

    # XXX: Make wrapper instead of symlink ? See ArchLinux's bastionmesa above.
    mkdir -p "$out/bin"
    ln -s "$dest/Bastion.bin.${arch}" "$out/bin/bastion"
  '';
}
