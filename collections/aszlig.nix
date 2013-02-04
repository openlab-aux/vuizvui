pkgs:

with pkgs;

{
  haskell = with haskellPackages; [
    ghc cabalInstall hlint
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
    zsh dash
    taskwarrior
    screen
    hexedit
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
    vim_configurable
    ltrace strace
  ];

  net = [
    netrw
    nmap
    socat
  ];

  x11 = [
    i3 i3lock i3status dmenu conky
    tkabber
    tkabber_plugins
    tkabber_urgent_plugin
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
