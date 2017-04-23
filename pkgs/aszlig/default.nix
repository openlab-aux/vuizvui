{ callPackage, callPackage_i686, boost155 }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  git-detach = callPackage ./git-detach { };
  grandpa = callPackage ./grandpa { };
  nixops = callPackage ./nixops { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  pvolctrl = callPackage ./pvolctrl { };
  santander = callPackage_i686 ./santander { };
  tomahawk = callPackage ./tomahawk { boost = boost155; };
}
