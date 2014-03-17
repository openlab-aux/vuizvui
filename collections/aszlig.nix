pkgs:

with pkgs;

{
  haskell = with haskellPackages; [
    ghc cabalInstall hlint
    cabal2nix
    darcs
    diagrams
    bytedump
    yesod yesodStatic yesodDefault yesodTest
    hjsmin persistentSqlite
  ];

  python = with pythonPackages; [
    pkgs.python3
    pkgs.python
    pep8
    polib
  ];

  games = [
    uqm
  ];

  shell = [
    ascii
    apg
    dash
    dos2unix
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
    p7zip
    rlwrap
    unzip
    vbindiff
  ];

  multimedia = [
    pulseaudio pvolctrl
    miro
    mplayer
    picard
    vorbisTools
    mpg321
    mp3info
    pavucontrol
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
    gitAndTools.hub
    nix-repl
    nixpkgs-lint
    ltrace strace
    valgrind
  ];

  dicts = with aspellDicts; [ de en ];

  net = [
    mtr
    netrw
    nmap
    socat
    samplicator
    jwhois
  ];

  x11 = [
    scrot
    i3 i3lock
    gajim
    xpdf
  ];

  misc = [
    lastwatch
    chromiumBetaWrapper
    firefox
    glxinfo
    imagemagick
    gimp
    graphviz
    youtubeDL
    ghostscript
    rtorrent
  ];
}
