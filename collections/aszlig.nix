pkgs:

{
  haskell = with pkgs.haskellPackages; [
    ghc cabalInstall hlint
    darcs
    diagrams
    yesod yesodStatic yesodDefault yesodTest
    hjsmin persistentSqlite
  ];

  python = with pkgs.pythonPackages; [
    pkgs.python3
    pkgs.python
    pep8
    polib
  ];

  shell = with pkgs; [
    zsh dash
    taskwarrior
    screen
    htop
    bc
    lftp
    mmv
    ncdu
    surfraw
    w3m
    fbida
    mutt
    vlock
  ];

  multimedia = with pkgs; [
    pulseaudio pvolctrl
    MPlayer
    vorbisTools
    mpg321
    mp3info
  ];

  crypto = with pkgs; [
    gnupg1compat openssh
    keychain
  ];

  dev = with pkgs; [
    manpages
    posix_man_pages
    gitFull
    gdb
    gnumake
    vim_configurable
    ltrace strace
  ];

  net = with pkgs; [
    netrw
    nmap
    socat
  ];

  x11 = with pkgs; [
    i3 i3lock i3status dmenu conky
    tkabber
    tkabber_plugins
    xpdf
  ];

  misc = with pkgs; [
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
