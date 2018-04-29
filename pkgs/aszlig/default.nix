{ callPackage, callPackage_i686, vim_configurable, xournal, gopass }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  grandpa = callPackage ./grandpa { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  pvolctrl = callPackage ./pvolctrl { };
  santander = callPackage_i686 ./santander { };
  vim = callPackage ./vim { vim = vim_configurable; };
  xournal = callPackage ./xournal { inherit xournal; };
}
