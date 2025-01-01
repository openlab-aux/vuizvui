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
    rev = "e91ae2bddafa0301e4a6265505ac14875e79d2c8";
    sha256 = "1ddknh7hrygzw7w2y314f9anyrqdrrc7z52ia8s9a6wbn0x4zkln";
  };

  nativeBuildInputs = [
    makeWrapper
    gobject-introspection
  ];

  propagatedBuildInputs = [
    pygobject3
  ];

  buildInputs = [
    gtk3
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
