{ fetchFromGitHub, buildPythonPackage, pythonPackages, cython, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchFromGitHub {
    owner = "aszlig";
    repo = "GrandPA";
    rev = "c8eb1fc40b20610380583444446b3e62b841df07";
    sha256 = "0mxf2ddqhbhcgblvivi0hslflabdj0qwyx92zbzayzgwlbqz4hss";
  };

  doCheck = false;

  buildInputs = [ cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    bsddb curses pyserial
  ];
}
