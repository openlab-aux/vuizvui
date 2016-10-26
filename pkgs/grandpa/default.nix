{ fetchFromGitHub, pythonPackages, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchFromGitHub {
    owner = "aszlig";
    repo = "GrandPA";
    rev = "d8d2571f732a68ed18be7533244db2cfb822b4c1";
    sha256 = "19zf3pnr1adngncvinvn8yyvc0sj66lp7lwiql6379rf78xxlmhn";
  };

  doCheck = false;

  buildInputs = [ pythonPackages.cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    curses pyserial
  ];
}
