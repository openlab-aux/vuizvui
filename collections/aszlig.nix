pkgs:

with pkgs;

{
  haskell = with haskellPackages; [
    ghc cabalInstall hlint
    cabal2nix
    darcs
    diagrams
    yesod yesodStatic yesodDefault yesodTest
    hjsmin persistentSqlite
  ];

  python = with pythonPackages; [
    pkgs.python3
    pkgs.python
    pep8
    polib
  ];

  shell = [
    dash
    taskwarrior
    screen
    figlet
    hexedit
    bc
    lftp
    mmv
    ncdu
    surfraw
    w3m
    fbida
    mutt
    rlwrap
  ];

  multimedia = [
    pulseaudio pvolctrl
    mplayer
    vorbisTools
    mpg321
    mp3info
  ];

  crypto = [
    gnupg1compat openssh
    keychain
  ];

  dev = [
    manpages
    posix_man_pages
    gitFull
    gdb
    gnumake
    ltrace strace
    valgrind
  ];

  net = [
    netrw
    nmap
    socat
    samplicator
    jwhois
  ];

  x11 = [
    i3 i3lock i3status dmenu conky
    gajim
    xpdf
  ];

  misc = [
    lastwatch
    chromiumBetaWrapper
    glxinfo
    imagemagick
    graphviz
    youtubeDL
    ghostscript
    rtorrent
  ];
}
