{ callPackage, callPackage_i686, vim_configurable, gopass, mutt }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  grandpa = callPackage ./grandpa { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  mutt = callPackage ./mutt { inherit mutt; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  pvolctrl = callPackage ./pvolctrl { };
  vim = callPackage ./vim { vim = vim_configurable; };
}
