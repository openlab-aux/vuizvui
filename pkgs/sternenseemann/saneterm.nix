{ buildPythonApplication
, lib
, fetchgit
, makeWrapper
, pygobject3
, gtk3
, gobject-introspection
, setuptools
}:

buildPythonApplication {
  pname = "saneterm";
  version = "git";

  pyproject = true;
  build-system = [ setuptools ];

  src = fetchgit {
    url = "https://git.8pit.net/saneterm.git";
    rev = "973cdccb4fd052641faafb3f352507efc4dd45bb";
    hash = "sha256-dKCRwwy9LHfFld55fIw+nlVSW+NmdDGLnGfzVa3WAAo=";
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
