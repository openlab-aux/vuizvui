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
    rev = "c7a0620eb2cca0595a57489d68aa8a535733b96d";
    sha256 = "1qp2kifbpp31nsw47f9zrggn8qvdh0x9n8hknv02vrkrhf9bd0ha";
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
