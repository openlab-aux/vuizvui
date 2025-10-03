{ callPackage, vim-full, gopass, mutt, xterm, radare2, rustfmt }:

{
  aacolorize = callPackage ./aacolorize { };
  firefox = callPackage ./firefox { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  lockdev = callPackage ./lockdev { };
  mutt = callPackage ./mutt { inherit mutt; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  vim = callPackage ./vim { vim = vim-full; };
  xterm = callPackage ./xterm { inherit xterm; };
}
