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
        nlSupport = true;
        pulseSupport = true;
        i3GapsSupport = true;
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
      alacritty
      antiword
      apg
      aria2
      ascii
      aspell
      aspellDicts.de
      aspellDicts.en
      axel
      bat
      bc
      bcal
      beets
      binutils
      bmon
      brave
      broot
      ccrypt
      chromaprint
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
      duf
      duff
      e2fsprogs
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
      gcc
      gdb
      ghostscript
      ghostwriter
      git
      gitinspector
      glow
      gnome3.adwaita-icon-theme
      gnome3.defaultIconTheme
      gnufdisk
      gnumake
      gnupg
      gopass
      gpgme
      gpodder
      gptfdisk
      graphviz
      hdparm
      hexedit
      hicolor-icon-theme
      hplipWithPlugin
      htop
      httpie
      i3lock-fancy
      imagemagick
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      irssi
      jfsutils
      john
      jwhois
      keepassxc
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
      oneshot
      openssl
      p7zip
      pamixer
      pandoc
      paperkey
      pbzip2
      pciutils
      pigz
      pipr
      pixz
      polybar
      posix_man_pages
      powertop
      profanity
      pulsemixer
      pv
      pxz
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
      signal-desktop
      smartmontools
      smos
      sox
      speedtest-cli
      spek
      ssdeep
      starship
      stow
      strace
      sxiv
      taizen
      taskell
      tasksh
      taskwarrior
      telnet
      termdown
      termite
      termshark
      testdisk
      tig
      tldr
      toilet
      transcode
      tree
      tty-clock
      ugrep
      units
      unrar
      unzip
      urlview
      usbutils
      valgrind
      vanilla-dmz
      vim_configurable
      virt-viewer
      virtmanager
      vit
      vlc
      vlock
      vorbisTools
      vorbisgain
      vscodium
      w3m
      wavpack
      wget
      which
      wipe
      wireguard
      wordgrinder
      xfsprogs
      xlibs.xev
      xscreensaver
      youtube-dl
      ytop
      zathura
      zbar
      zgrviewer
      zip
      zotero
      zstd
      zsync
    ];
  };
}
