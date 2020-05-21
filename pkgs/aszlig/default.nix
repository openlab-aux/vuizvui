{ callPackage, callPackage_i686, vim_configurable, gopass }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  grandpa = callPackage ./grandpa { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  psi = callPackage ./psi { };
  pvolctrl = callPackage ./pvolctrl { };
  vim = callPackage ./vim { vim = vim_configurable; };
}
