{ stdenv, fetchurl, autoconf, ncurses, groff, makeWrapper, pythonPackages }:

stdenv.mkDerivation rec {
  name = "fish-${version}";
  version = "2.0.0";

  buildInputs = [
    autoconf ncurses makeWrapper pythonPackages.wrapPython pythonPackages.python
  ];

  pythonPath = [ pythonPackages.curses ];

  enableParallelBuilding = true;

  preConfigure = ''
    autoconf
    sed -i 's/\<which\>/type -P/g' configure
  '';

  postFixup = ''
    wrapPythonPrograms

    sed -i 's|nroff|${groff}/bin/nroff|g' \
      "$out/share/fish/functions/__fish_print_help.fish"
  '';

  src = fetchurl {
    url = "http://fishshell.com/files/${version}/fish.tar.gz";
    sha1 = "2d28553e2ff975f8e5fed6b266f7a940493b6636";
  };
}
