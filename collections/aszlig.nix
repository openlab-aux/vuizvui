pkgs:

with pkgs;

{
  haskell = with haskellPackages; [
    cabal2nix
    hlint
    yesodBin
  ];

  python = with pythonPackages; [
    hetzner
    pep8
    polib
    pkgs.python
    pkgs.python3
  ];

  games = [
    uqm
  ];

  shell = [
    apg
    ascii
    bc
    dash
    dos2unix
    fbida
    figlet
    hexedit
    lftp
    mmv
    mutt
    ncdu
    openssl
    p7zip
    rlwrap
    screen
    surfraw
    taskwarrior
    unzip
    vbindiff
    w3m
  ];

  multimedia = [
    ffmpeg
    flac
    miro
    mp3info
    mpg321
    mplayer
    pavucontrol
    picard
    pulseaudio
    pvolctrl
    vorbisTools
  ];

  crypto = [
    gnupg1compat
    keychain
    openssh
  ];

  dev = [
    erlang
    gdb
    gitAndTools.hub
    gitFull
    gnumake
    haxe
    ltrace
    manpages
    neko
    nix-prefetch-scripts
    nix-repl
    nixpkgs-lint
    posix_man_pages
    strace
    valgrind
  ];

  dicts = with aspellDicts; [ de en ];

  net = [
    jwhois
    mtr
    netrw
    nixops
    nmap
    samplicator
    socat
    telnet
    wireshark
  ];

  x11 = [
    gajim
    i3
    i3lock
    scrot
    xpdf
  ];

  misc = [
    axbo
    (chromiumBeta.override {
      enablePepperFlash = true;
    })
    firefox
    ghostscript
    gimp
    glxinfo
    graphviz
    imagemagick
    lastwatch
    rtorrent
    sqlite
    youtubeDL
  ];
}
