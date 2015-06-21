{ config, pkgs, lib, ... }:

{
  nixpkgs.config = {
    pulseaudio = true;

    allowUnfree = true;

    conky = {
      weatherMetarSupport = true;
      mpdSupport = true;
      wirelessSupport = true;
      x11Support = false;
      alsaSupport = false;
    };

    virtualbox = {
      enableExtensionPack = false;
    };

    mpv = {
      youtubeSupport = true;
    };
  };

  environment.systemPackages = with pkgs; [
    #(lib.overrideDerivation mcabber (o: { buildInputs = (o.buildInputs or []) ++ lib.singleton pkgs.gpgme; }))
    abook
    accountsservice
    apg
    ascii
    aspell
    aspellDicts.de
    aspellDicts.en
    attic
    audacity
    axel
    bc
    biber
    bind
    bup
    cacert
    ccrypt
    chromaprint
    chromium
    cifs_utils
    cmake
    colordiff
    conky
    cryptsetup
    ctodo
    cuetools
    dcfldd
    ddrescue
    dmenu
    dmidecode
    dos2unix
    easytag
    emacs
    enhanced-ctorrent
    ethtool
    evince
    fbida
    feh
    ffmpeg
    figlet
    file
    firefox
    flac
    freerdpUnstable
    gajim
    gcc
    gdb
    ghostscript
    gimp
    gitAndTools.darcsToGit
    gitAndTools.git-annex
    gitAndTools.git-extras
    gitAndTools.git-remote-hg
    gitAndTools.git2cl
    gitAndTools.gitFastExport
    gitAndTools.gitFull
    gitAndTools.gitRemoteGcrypt
    gitAndTools.gitSVN
    gitAndTools.gitflow
    gitAndTools.svn2git
    gitAndTools.tig
    glxinfo
    gnome3.dconf
    gnome3.defaultIconTheme
    gnome3.gnome_themes_standard
    gnufdisk
    gnupg
    gnupg1compat
    gource
    gpgme
    gptfdisk
    graphviz
    gstreamer
    hdparm
    hexedit
    htop
    i3lock
    i3status
    icedtea_web
    iftop
    imagemagick
    impressive
    inkscape
    iotop
    ipv6calc
    jwhois
    keepassx
    lftp
    libarchive
    libreoffice
    lm_sensors
    lsof
    lxappearance
    lxc
    lynx
    manpages
    mcabber
    mediainfo
    mmv
    monkeysAudio
    mono
    monodevelop
    mosh
    mp3gain
    mpc_cli
    mpv
    mtr
    ncdu
    ncmpcpp
    netrw
    nitrogen
    nix-prefetch-scripts
    nixops
    nmap
    obnam
    openssl
    p7zip
    pandoc
    pass
    pasystray
    pavucontrol
    picard
    posix_man_pages
    powertop
    profanity
    profile-cleaner
    profile-sync-daemon
    pv
    python
    python2
    python3
    pythonPackages.rainbowstream
    recode
    ruby
    screen
    scrot
    shntool
    silver-searcher
    sleuthkit
    smartmontools
    sox
    spek
    stow
    strace
    surfraw
    taskwarrior
    telnet
    texLiveFull
    tomahawk
    transmission_remote_gtk
    tree
    tribler
    units
    unrar
    unzip
    vim_configurable
    vlock
    vorbisTools
    vorbisgain
    w3m
    wavpack
    weechat
    wget
    which
    wireshark
    xlibs.xev
    xpdf
    xpra
    xscreensaver
    youtube-dl
    zotero
    zsync
  ];
}
