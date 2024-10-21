{ callPackage, vim-full, gopass, gpodder, mutt, xterm, radare2
, rustfmt
}:

{
  aacolorize = callPackage ./aacolorize { };
  firefox = callPackage ./firefox { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  gpodder = callPackage ./gpodder { inherit gpodder; };
  lockdev = callPackage ./lockdev { };
  mutt = callPackage ./mutt { inherit mutt; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  pvolctrl = callPackage ./pvolctrl { };
  vim = callPackage ./vim { vim = vim-full; };
  xterm = callPackage ./xterm { inherit xterm; };
}
