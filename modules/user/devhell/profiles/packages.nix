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
      #arc-theme
      #atftp
      #bup
      #cacert
      #texlive.combined.scheme-small
      #tftp-hpa
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
      bcal
      beets
      binutils
      bmon
      cataclysm-dda
      ccrypt
      chromaprint
      chromium
      cifs_utils
      cipherscan
      clac
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
      di
      dmenu
      dmidecode
      dos2unix
      dosbox
      duff
      e2fsprogs
      easytag
      electricsheep
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
      focuswriter
      fortune
      freerdpUnstable
      fuse_exfat
      fzf
      gajim
      gcc
      gdb
      ghostscript
      ghostwriter
      git
      gitinspector
      gnufdisk
      gnumake
      gnupg
      gource
      gparted
      gpgme
      gptfdisk
      graphviz
      gstreamer
      hdparm
      hexedit
      hplipWithPlugin
      htop
      i3lock
      i3lock-color
      i3status
      iftop
      imagemagick
      iotop
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      jfsutils
      john
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
      mediainfo
      mkvtoolnix
      mmv
      monkeysAudio
      mosh
      mp3gain
      mpc_cli
      mpv
      msmtp
      mtr
      ncdu
      ncmpcpp
      neofetch
      neomutt
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
      notmuch
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
      reptyr
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
      tig
      timewarrior
      tldr
      tmux
      toilet
      transcode
      transgui
      tree
      tty-clock
      units
      unrar
      unzip
      urlview
      usbutils
      valgrind
      vanilla-dmz
      vim_configurable
      virt-viewer
      virtinst
      virtmanager
      vit
      vlc
      vlock
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
