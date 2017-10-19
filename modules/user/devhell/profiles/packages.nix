{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.devhell.profiles.packages;

in {
  options.vuizvui.user.devhell.profiles.packages = {
    enable = lib.mkEnableOption "Packages profile for devhell";
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = lib.singleton (lib.const (super: {
      ncmpcpp = super.ncmpcpp.override {
        visualizerSupport = true;
        clockSupport = true;
      };

      sox = super.sox.override {
        enableLame = true;
      };
    }));

    nixpkgs.config = {
      pulseaudio = true;

      allowUnfree = true;

      systemd = {
        enableKDbus = true;
      };

      conky = {
        weatherMetarSupport = true;
        mpdSupport = true;
        wirelessSupport = true;
        x11Support = false;
      };

      firefox = {
        enableGTK3 = true;
        enableOfficalBranding = true;
      };

      virtualbox = {
        enableExtensionPack = true;
      };

      mpv = {
        youtubeSupport = true;
      };
    };

    environment.systemPackages = with pkgs; [
      #arc-theme
      #attic
      #emacs
      #gitAndTools.git-annex
      #ipfs
      #john
      #lxappearance
      #sleuthkit
      #texmacs
      #tribler
      #vimiv
      #zotero
      abcde
      abook
      accountsservice
      antiword
      apg
      arandr
      ascii
      aspell
      aspellDicts.de
      aspellDicts.en
      atftp
      atom
      audacity
      axel
      bc
      beets
      biber
      bind
      binutils
      brotli
      bup
      cacert
      cataclysm-dda
      cava
      ccrypt
      chromaprint
      chromium
      cifs_utils
      cipherscan
      cmake
      cmatrix
      colordiff
      compton
      conky
      cryptsetup
      ctodo
      cuetools
      darkstat
      dcfldd
      ddrescue
      dhcping
      dmenu
      dmidecode
      docker
      dos2unix
      dosbox
      duff
      dynamic-colors
      e2fsprogs
      easytag
      electrum
      enhanced-ctorrent
      ethtool
      evince
      fbida
      fdupes
      feh
      ffmpeg-full
      figlet
      file
      firefox
      flac
      foremost
      freerdpUnstable
      fuse_exfat
      gajim
      gcc
      gdb
      ghostscript
      gimp
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
      gparted
      gpgme
      gpicview
      gptfdisk
      graphviz
      gstreamer
      handbrake
      hdparm
      heimdall
      hexedit
      hplipWithPlugin
      htop
      i3lock
      i3lock-color
      i3status
      icedtea_web
      iftop
      imagemagick
      impressive
      inkscape
      iotop
      iptraf-ng
      ipv6calc
      jfsutils
      jrnl
      jwhois
      keepassx
      keepassx-community
      keepassx2
      keybase
      kpcli
      lftp
      libarchive
      libreoffice
      lm_sensors
      lsof
      lxc
      lynx
      macchanger
      manpages
      mc
      mcabber
      mdp
      mediainfo
      mkvtoolnix
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
      neovim
      nethack
      nethogs
      netkittftp
      netrw
      netsniff-ng
      nitrogen
      nix-prefetch-scripts
      nix-repl
      nixops
      nload
      nmap
      ntfs3g
      ntfsprogs
      ntopng
      numix-icon-theme
      obnam
      openssl
      p7zip
      pandoc
      paperkey
      pass
      pasystray
      pavucontrol
      pciutils
      picard
      polybar
      posix_man_pages
      powertop
      profanity
      profile-cleaner
      profile-sync-daemon
      pv
      python
      python2
      python3
      python34Packages.hovercraft
      pythonPackages.glances
      pythonPackages.livestreamer
      pythonPackages.rainbowstream
      qemu
      qrencode
      recode
      reiserfsprogs
      rofi
      rsync
      ruby
      safecopy
      screen
      scrot
      shntool
      silver-searcher
      smartmontools
      sox
      speedtest-cli
      spek
      ssdeep
      stow
      strace
      surfraw
      taskwarrior
      telnet
      termite
      testdisk
      texlive.combined.scheme-small
      tftp-hpa
      tldr
      tmux
      toilet
      tomahawk
      toxic
      transcode
      transgui
      transmission_remote_gtk
      tree
      tty-clock
      udevil
      units
      unrar
      unzip
      valgrind
      vanilla-dmz
      vim_configurable
      virt-viewer
      virtinst
      virtmanager
      vit
      vlc
      vlock
      vnstat
      vorbisTools
      vorbisgain
      w3m
      wavpack
      weechat
      wget
      which
      wipe
      wireshark
      xfsprogs
      xlibs.xev
      xmpp-client
      xpdf
      xpra
      xscreensaver
      youtube-dl
      zathura
      zbar
      zip
      zsync
    ];
  };
}
