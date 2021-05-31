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
    rev = "d96c2422b67e8c92a41e1f87813ebc1a839d5d69";
    sha256 = "1546mds8c3yw418pqbnsjxv00lrspfdf74pwi31yqhmphbpzl6pf";
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
