{ callPackage, vim-full, anyrun, gopass, ironbar, kitty, mako, mutt, niri
, radare2, rustfmt
}:

{
  aacolorize = callPackage ./aacolorize { };
  anyrun = callPackage ./anyrun { inherit anyrun; };
  firefox = callPackage ./firefox { };
  git-detach = callPackage ./git-detach { };
  gopass = callPackage ./gopass { inherit gopass; };
  ironbar = callPackage ./ironbar { inherit ironbar; };
  kitty = callPackage ./kitty { inherit kitty; };
  lockdev = callPackage ./lockdev { };
  mako = callPackage ./mako { inherit mako; };
  mutt = callPackage ./mutt { inherit mutt; };
  niri = callPackage ./niri { inherit niri; };
  nlast = callPackage ./nlast { };
  psi = callPackage ./psi { };
  vim = callPackage ./vim { vim = vim-full; };
}
