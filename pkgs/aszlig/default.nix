{ callPackage, vim_configurable, gopass, mutt, xterm, rustfmt }:

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
  rustfmt = callPackage ./rustfmt { inherit rustfmt; };
  vim = callPackage ./vim { vim = vim_configurable; };
  xterm = callPackage ./xterm { inherit xterm; };
}
