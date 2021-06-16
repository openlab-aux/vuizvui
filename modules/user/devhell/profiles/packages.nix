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

    environment.etc."mdadm.conf".text = ''
      MAILADDR root
    '';

    environment.systemPackages = with pkgs; [
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
      bandwhich
      bat
      bc
      bcal
      beets
      binutils
      blender
      bmon
      bottom
      brave
      broot
      catgirl
      cbonsai
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
      delta
      dhcping
      di
      dmidecode
      dogdns
      dos2unix
      duf
      duff
      e2fsprogs
      element-desktop
      ethtool
      fd
      fdupes
      feh
      ffmpeg-full
      figlet
      file
      firefox
      flac
      flameshot
      flavours
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
      gitAndTools.gh
      gitinspector
      gitui
      glow
      gnome.adwaita-icon-theme
      gnome.defaultIconTheme
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
      hexyl
      hicolor-icon-theme
      hplipWithPlugin
      htop
      httpie
      imagemagick
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      jfsutils
      jwhois
      kmon
      ldns
      lftp
      libarchive
      libreoffice
      lm_sensors
      lsof
      lxc
      maim
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
      ncdu
      ncmpcpp
      neofetch
      neomutt
      nethack
      nethogs
      netrw
      netsniff-ng
      nitrogen
      nixfmt
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
      pastel
      pbzip2
      pciutils
      pigz
      pipr
      pixz
      podman
      podman-compose
      posix_man_pages
      powertop
      profanity
      pulsemixer
      pv
      pxz
      qemu
      qrencode
      radeontop
      recode
      reptyr
      ripgrep
      rsync
      safecopy
      screen
      signal-desktop
      smartmontools
      smos
      so
      sox
      speedtest-cli
      spek
      ssdeep
      starship
      stow
      strace
      sxiv
      tasksh
      taskwarrior
      taskwarrior-tui
      tealdeer
      telnet
      termdown
      termshark
      testdisk
      tig
      toilet
      tree
      tty-clock
      ugrep
      units
      unrar
      unzip
      urlview
      usbutils
      vanilla-dmz
      virt-viewer
      virtmanager
      visidata
      vivid
      vlc
      vlock
      vorbisTools
      vorbisgain
      vscodium
      vuizvui.devhell.vim
      w3m
      wavpack
      wget
      which
      wipe
      wireguard
      wordgrinder
      xfsprogs
      xlibs.xev
      youtube-dl
      zathura
      zbar
      zellij
      zettlr
      zip
      zotero
      zstd
    ];
  };
}
