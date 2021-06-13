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
    rev = "0288f7436406af694d56b5656c4a8a92b860f82b";
    sha256 = "0pwadkrdajxsm6vj16nvkqdjfv4mz3ffsfg55l99jilil1dk0xx7";
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
