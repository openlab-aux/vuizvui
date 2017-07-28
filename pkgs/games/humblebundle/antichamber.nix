{ stdenv, lib, fetchHumbleBundle, unzip
, xorg, libpulseaudio, libvorbis, libogg, mesa }:

stdenv.mkDerivation rec {
  name = "antichamber-1.1";

  src = fetchHumbleBundle {
    name = "antichamber_1.01_linux_1392664980.sh";
    machineName = "antichamber_linux";
    md5 = "37bca01c411d813c8729259b7db2dba0";
  };

  dontStrip = true;
  phases = ["installPhase"];

  installPhase = let
    rpath = lib.makeLibraryPath [
      "$dest/Binaries/Linux"
      xorg.libX11
      xorg.libXi
      stdenv.cc.cc.lib
      libpulseaudio
      libvorbis
      libogg
      mesa
    ];
   in ''
    dest="$out/share/antichamber"

    # Unpack binaries and data into $dest
    mkdir -p "$dest"
    ${unzip}/bin/unzip $src "data/*" -d $dest && [ $? -le 2 ]
    mv $dest/data/*/* $dest
    rm -r $dest/data

    # Patch heavily :-)
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${rpath}" "$dest/Binaries/Linux/UDKGame-Linux"
    for exe in $dest/Binaries/Linux/lib/*.so{,.*} ; do
      patchelf --set-rpath "${rpath}" "$exe"
    done

    # Fixup permissions, just to be sure.
    find "$dest" -type f -exec chmod 644 "{}" +
    find "$dest/Binaries" -type f -exec chmod 755 "{}" +

    mkdir -p "$out/bin"
    cat > $out/bin/antichamber <<EOF
    #!${stdenv.shell}
    cd $dest/Binaries/Linux/
    exec ./UDKGame-Linux "$@"
    EOF
    chmod 755 $out/bin/antichamber
  '';
}
