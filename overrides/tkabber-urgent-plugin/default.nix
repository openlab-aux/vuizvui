{ stdenv, fetchsvn, xlibs }:

stdenv.mkDerivation {
  name = "tkabber-urgent-plugin";

  src = fetchsvn {
    url = "http://svn.xmpp.ru/repos/tkabber-3rd-party/trunk/plugins/urgent";
    rev = 528;
    sha256 = "1qr7i0559ad5y1l5h2gp8aix4nsfgm0bx7jqb030hgbxaw1xnbp5";
  };

  buildInputs = [ xlibs.libX11 ];

  patchPhase = ''
    sed -i -e 's|exec xwininfo|exec ${xlibs.xwininfo}/bin/xwininfo|' urgent.tcl
  '';

  buildPhase = ''
    gcc -lX11 -o urgent urgent.c
  '';

  installPhase = ''
    install -vd "$out/share/tkabber-plugins/urgent"
    cp -vpt "$out/share/tkabber-plugins/urgent" urgent urgent.tcl
  '';
}
