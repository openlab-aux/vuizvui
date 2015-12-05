{ fetchFromGitHub, buildPythonPackage, pythonPackages, cython, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchFromGitHub {
    owner = "aszlig";
    repo = "GrandPA";
    rev = "b2c4a653e4ea37f751e07c96f2f6e7d0337356cf";
    sha256 = "0zg4ndbq6rw67zz4pvwdw1ws4sywacx1m5q1yjsbvbapp32szbv0";
  };

  doCheck = false;

  buildInputs = [ cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    bsddb curses pyserial
  ];
}
