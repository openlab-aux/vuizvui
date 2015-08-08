{ fetchFromGitHub, buildPythonPackage, pythonPackages, cython, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchFromGitHub {
    owner = "aszlig";
    repo = "GrandPA";
    rev = "a9bb455a6789c4b9bacd27ab9d3cdb7b141c29d8";
    sha256 = "15aqpinrwsg53g1aprkqqp1sr5ch4q8zpm982s228i38vdzp8i60";
  };

  doCheck = false;

  buildInputs = [ cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    bsddb curses pyserial
  ];
}
