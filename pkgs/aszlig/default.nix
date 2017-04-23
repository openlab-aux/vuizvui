{ callPackage, callPackage_i686, boost155, gajim }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  gajim = callPackage ./gajim { inherit gajim; };
  git-detach = callPackage ./git-detach { };
  grandpa = callPackage ./grandpa { };
  nixops = callPackage ./nixops { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  pvolctrl = callPackage ./pvolctrl { };
  santander = callPackage_i686 ./santander { };
  tomahawk = callPackage ./tomahawk { boost = boost155; };
}
