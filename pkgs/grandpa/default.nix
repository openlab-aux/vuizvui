{ fetchgit, buildPythonPackage, pythonPackages, cython, gpm }:

pythonPackages.buildPythonPackage {
  name = "grandpa-0.5";
  namePrefix = "";

  src = fetchgit {
    url = "git://github.com/aszlig/GrandPA.git";
    rev = "41f21f67446b98f4600bc043abe32b95af6dd048";
    sha256 = "06sgz39ri0ai3x2fck44rczk04pj8zcysbhp97x20ggmqhx5vxyh";
  };

  doCheck = false;

  buildInputs = [ cython gpm ];
  propagatedBuildInputs = with pythonPackages; [
    bsddb curses pyserial pyglet
  ];
}
