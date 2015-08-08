{ fetchFromGitHub, buildPythonPackage, pythonPackages, cython, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchFromGitHub {
    owner = "aszlig";
    repo = "GrandPA";
    rev = "a01900bc9921a102f85de011d1c655b2d831714c";
    sha256 = "1ynvsn74djdjmyybbpfzdc7m2zkz5raph7nfyklx8alsbwr8n40s";
  };

  doCheck = false;

  buildInputs = [ cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    bsddb curses pyserial
  ];
}
