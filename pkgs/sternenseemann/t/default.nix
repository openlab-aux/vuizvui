{ lib, buildPythonApplication, fetchFromGitHub, cram }:

buildPythonApplication rec {
  pname = "t";
  version = "unstable-2020-04-11";

  src = fetchFromGitHub {
    owner  = "sjl";
    repo   = pname;
    rev    = "815ccaf4f0bf2acb2a7f2cb330bf0532d782f408";
    sha256 = "1bi2hpdgwbqhcy88laba7h6kiqxvz75qqaf6sq221n39zfdl8n1g";
  };

  meta = with lib; {
    license = licenses.mit; # x11 license
    description = "A command-line todo list manager for people that want to finish tasks, not organize them";
    homepage = "https://hg.stevelosh.com/t";
  };
}
