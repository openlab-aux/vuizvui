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

      mpv = {
        youtubeSupport = true;
      };
    };

    environment.systemPackages = with pkgs; [
      #electricsheep
      #ntopng
      #texlive.combined.scheme-small
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
      cryptsetup
      cuetools
      darkstat
      dcfldd
      ddrescue
      dhcping
      di
      dmidecode
      dos2unix
      duff
      e2fsprogs
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
      gopass
      gotop
      gource
      gparted
      gpgme
      gpodder
      gptfdisk
      graphviz
      gstreamer
      hdparm
      hexedit
      hplipWithPlugin
      htop
      i3lock-color
      i3lock-fancy
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
      ldns
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
      nixops
      nload
      nmap
      ntfs3g
      ntfsprogs
      openssl
      p7zip
      pandoc
      paperkey
      pbzip2
      pciutils
      pigz
      pixz
      polybarFull
      posix_man_pages
      powertop
      profanity
      pulsemixer
      pv
      pxz
      python
      python2
      python3
      qemu
      qrencode
      recode
      reptyr
      ripgrep
      rofi
      rstudio
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
      telnet
      termdown
      termite
      testdisk
      tig
      tldr
      tmux
      toilet
      transcode
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
      vscodium
      w3m
      wavpack
      weechat
      wget
      which
      wipe
      wireguard
      wireshark
      wordgrinder
      xfsprogs
      xlibs.xev
      xscreensaver
      youtube-dl
      zathura
      zbar
      zgrviewer
      zip
      zotero
      zsync
    ];
  };
}
