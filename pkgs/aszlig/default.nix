{ callPackage, vim-full, gopass, mutt, xterm, radare2, rustfmt }:

{
  aacolorize = callPackage ./aacolorize { };
  axbo = callPackage ./axbo { };
  firefox = callPackage ./firefox { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  grandpa = callPackage ./grandpa { };
  librxtx_java = callPackage ./librxtx-java { };
  lockdev = callPackage ./lockdev { };
  mutt = callPackage ./mutt { inherit mutt; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  pvolctrl = callPackage ./pvolctrl { };
  radare2 = callPackage ./radare2 { inherit radare2; };
  rustfmt = callPackage ./rustfmt { inherit rustfmt; };
  vim = callPackage ./vim { vim = vim-full; };
  xterm = callPackage ./xterm { inherit xterm; };
}
