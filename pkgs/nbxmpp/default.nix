{ stdenv, fetchurl, buildPythonPackage }:

buildPythonPackage rec {
  name = "nbxmpp-0.4";

  src = fetchurl {
    name = "${name}.tar.gz";
    url = "https://python-nbxmpp.gajim.org/downloads/4";
    sha256 = "073qhz6vgsym4rkgwj801girl8xqhlbr37xn5lg0za9c61vmwayh";
  };

  meta = {
    homepage = "https://python-nbxmpp.gajim.org/";
    description = "Non-blocking Jabber/XMPP module";
    license = stdenv.lib.licenses.gpl3;
  };
}
