{ callPackage, callPackage_i686, boost155, gajim, vim_configurable, xournal }:

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
  vim = callPackage ./vim { vim = vim_configurable; };
  xournal = callPackage ./xournal { inherit xournal; };
}
