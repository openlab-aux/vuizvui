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
      bc
      bcal
      beets
      binutils
      bmon
      broot
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
      gcc
      gdb
      ghostscript
      ghostwriter
      git
      gitinspector
      glow
      gnufdisk
      gnumake
      gnupg
      gopass
      gotop
      gpgme
      gpodder
      gptfdisk
      graphviz
      gstreamer
      hdparm
      hexedit
      hplipWithPlugin
      htop
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
      smartmontools
      sox
      speedtest-cli
      spek
      ssdeep
      starship
      stow
      strace
      sxiv
      taskell
      tasksh
      taskwarrior
      taizen
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
      (virtinst.override {
        python2Packages = python2Packages.override {
          overrides = lib.const (super: {
            routes = super.routes.overridePythonAttrs (lib.const {
              doCheck = false;
            });
          });
        };
      })
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
