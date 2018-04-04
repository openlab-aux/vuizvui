{ callPackage, callPackage_i686, vim_configurable, xournal }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  git-detach = callPackage ./git-detach { };
  grandpa = callPackage ./grandpa { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  pvolctrl = callPackage ./pvolctrl { };
  santander = callPackage_i686 ./santander { };
  vim = callPackage ./vim { vim = vim_configurable; };
  xournal = callPackage ./xournal { inherit xournal; };
}
