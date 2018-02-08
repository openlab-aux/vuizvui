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

      polybar = super.polybar.override {
        i3Support = true;
        mpdSupport = true;
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
      #arandr
      #arc-theme
      #atftp
      #atom
      #attic
      #audacity
      #biber
      #bind
      #brotli
      #bup
      #cacert
      #cava
      #cmake
      #compton
      #docker
      #emacs
      #gimp
      #gitAndTools.git-annex
      #gitAndTools.git-extras
      #gitAndTools.git-remote-hg
      #gitAndTools.git2cl
      #gitAndTools.gitFastExport
      #gitAndTools.gitFull
      #gitAndTools.gitRemoteGcrypt
      #gitAndTools.gitSVN
      #gitAndTools.gitflow
      #gitAndTools.svn2git
      #gitAndTools.tig
      #gnome3.dconf
      #gnome3.defaultIconTheme
      #gnome3.gnome_themes_standard
      #heimdall
      #inkscape
      #ipfs
      #kpcli
      #mc
      #netkittftp
      #numix-icon-theme
      #pasystray
      #pavucontrol
      #python34Packages.hovercraft
      #ruby
      #sleuthkit
      #surfraw
      #texlive.combined.scheme-small
      #texmacs
      #tftp-hpa
      #tomahawk
      #toxic
      abcde
      abook
      accountsservice
      ag
      antiword
      apg
      ascii
      aspell
      aspellDicts.de
      aspellDicts.en
      axel
      bc
      beets
      binutils
      cataclysm-dda
      ccrypt
      chromaprint
      chromium
      cifs_utils
      cipherscan
      cmatrix
      colordiff
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
      dos2unix
      dosbox
      duff
      e2fsprogs
      easytag
      electrum
      enhanced-ctorrent
      ethtool
      fbida
      fd
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
      fzf
      gajim
      gcc
      gdb
      ghostscript
      git
      glxinfo
      gnufdisk
      gnupg
      gource
      gparted
      gpgme
      gptfdisk
      graphviz
      gstreamer
      handbrake
      hdparm
      hexedit
      hplipWithPlugin
      htop
      i3lock
      i3lock-color
      i3status
      icedtea_web
      iftop
      imagemagick
      iotop
      ipcalc
      iptraf-ng
      ipv6calc
      jfsutils
      john
      jrnl
      jwhois
      keepassxc
      keybase
      lftp
      libarchive
      libreoffice
      lm_sensors
      lsof
      lxc
      lynx
      macchanger
      manpages
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
      obnam
      openssl
      p7zip
      pandoc
      paperkey
      pass
      pbzip2
      pciutils
      pigz
      pixz
      polybar
      posix_man_pages
      powertop
      profanity
      profile-cleaner
      profile-sync-daemon
      pulsemixer
      pv
      pxz
      python
      python2
      python3
      pythonPackages.glances
      pythonPackages.livestreamer
      pythonPackages.rainbowstream
      qemu
      qrencode
      recode
      reiserfsprogs
      ripgrep
      rofi
      rsync
      safecopy
      screen
      scrot
      shntool
      smartmontools
      sox
      speedtest-cli
      spek
      ssdeep
      stow
      strace
      sxiv
      tasksh
      taskwarrior
      tectonic
      telnet
      termdown
      termite
      testdisk
      timewarrior
      tldr
      tmux
      toilet
      transcode
      transgui
      tree
      tribler
      tty-clock
      udevil
      udiskie
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
      weather
      weechat
      wget
      which
      wipe
      wireshark
      xfsprogs
      xlibs.xev
      xmpp-client
      xpra
      xscreensaver
      youtube-dl
      zathura
      zbar
      zip
      zotero
      zsync
    ];
  };
}
