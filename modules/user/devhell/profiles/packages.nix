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

#      virtualbox = {
#        enableExtensionPack = true;
#      };

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
      #sleuthkit
      #texmacs
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
      gource
      gparted
      gpgme
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
      inkscape
      iotop
      iptraf-ng
      ipv6calc
      jfsutils
      jrnl
      jwhois
      keepassxc
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
      polybar
      posix_man_pages
      powertop
      profanity
      profile-cleaner
      profile-sync-daemon
      pulsemixer
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
      ripgrep
      rofi
      rsync
      ruby
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
      surfraw
      taskwarrior
      tectonic
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
      zsync
    ];
  };
}
