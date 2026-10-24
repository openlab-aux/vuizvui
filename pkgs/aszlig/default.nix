{ callPackage, vim-full, gopass, kitty, mutt, niri, xterm, radare2, rustfmt }:

{
  aacolorize = callPackage ./aacolorize { };
  firefox = callPackage ./firefox { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  kitty = callPackage ./kitty { inherit kitty; };
  lockdev = callPackage ./lockdev { };
  mutt = callPackage ./mutt { inherit mutt; };
  niri = callPackage ./niri { inherit niri; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  vim = callPackage ./vim { vim = vim-full; };
  xterm = callPackage ./xterm { inherit xterm; };
}
