{ stdenv, fetchurl, autoconf, ncurses, groff, pythonPackages }:

stdenv.mkDerivation rec {
  name = "fish-${version}";
  version = "2.0.0";

  buildInputs = [ autoconf ncurses pythonPackages.python ];

  pythonPath = [ pythonPackages.curses ];

  enableParallelBuilding = true;

  preConfigure = ''
    autoconf
    sed -i 's/\<which\>/type -P/g' configure
  '';

  postFixup = ''
    gen_inserts() {
      for i in $pythonPath; do
        echo -n "; sys.path.insert(0, '$(toPythonPath "$i")')"
      done
    }

    find "$out/share/fish/tools" -type f -perm +0100 -name '*.py' |
    xargs sed -i -r \
      -e '1 { /^#!/c #!${pythonPackages.python}/bin/python
            }' \
      -e '0,/^(from|import)/{/^(from|import)/a import sys'"$(gen_inserts)"'
                            }'

    sed -i -e "/Popen/s/'manpath'/'man', '-w'/p" \
      "$out/share/fish/tools/create_manpage_completions.py"

    sed -i -e 's|nroff|${groff}/bin/nroff|g' \
      "$out/share/fish/functions/__fish_print_help.fish"
  '';

  src = fetchurl {
    url = "http://fishshell.com/files/${version}/fish.tar.gz";
    sha1 = "2d28553e2ff975f8e5fed6b266f7a940493b6636";
  };
}
