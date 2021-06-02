{ buildPythonApplication
, lib
, fetchgit
, makeWrapper
, pygobject3
, gtk3
, gobject-introspection
}:

buildPythonApplication {
  pname = "saneterm";
  version = "git";

  src = fetchgit {
    url = "git://git.8pit.net/saneterm.git";
    rev = "15448192c2b905a43c8b009ffeca0d6fc74fb639";
    sha256 = "04gaclfbvm7gqjb588x3g69gygb0q9lsz2shq9b2lj63lrpwh7r0";
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  propagatedBuildInputs = [
    pygobject3
  ];

  buildInputs = [
    gtk3
    gobject-introspection
  ];

  postInstall = ''
    wrapProgram "$out/bin/saneterm" \
      --prefix GI_TYPELIB_PATH : "$GI_TYPELIB_PATH" \
      --prefix LD_LIBRARY_PATH ":" "${lib.getLib gtk3}/lib"
  '';

  meta = {
    description = "Modern line-oriented terminal emulator without support for TUIs";
    homepage = "https://git.8pit.net/saneterm/";
    license = lib.licenses.gpl3Only;
  };
}
