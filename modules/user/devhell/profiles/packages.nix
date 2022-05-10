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
      allowUnfree = true;

      systemd = {
        enableKDbus = true;
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
      alacritty
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
      borgbackup
      bottom
      brave
      broot
      catgirl
      cbonsai
      ccrypt
      chromaprint
      cifs-utils
      cmatrix
      colmena
      cryptsetup
      cuetools
      dcfldd
      ddrescue
      delta
      dhcping
      di
      diskus
      dmidecode
      dogdns
      dos2unix
      du-dust
      dua
      duf
      e2fsprogs
      easyeffects
      element-desktop
      ethtool
      exfat
      fd
      feh
      ffmpeg-full
      figlet
      file
      firefox
      flac
      flameshot
      flavours
      fortune
      freerdpUnstable
      fzf
      gcc
      gdb
      gh
      ghostscript
      git
      git-trim
      gitui
      glow
      gnome.adwaita-icon-theme
      gnufdisk
      gnumake
      gnupg
      gomuks
      gopass
      gpg-tui
      gpgme
      gpodder
      gptfdisk
      graphviz
      grex
      grv
      gurk-rs
      handlr
      hdparm
      hexedit
      hexyl
      hicolor-icon-theme
      hplipWithPlugin
      htop
      imagemagick
      imv
      inetutils
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      jq
      jwhois
      kmon
      ldns
      lfs
      lftp
      libarchive
      libreoffice
      librespeed-cli
      lm_sensors
      lsd
      lsof
      lxd
      maim
      man-pages
      mapcidr
      mediainfo
      menyoki
      mkvtoolnix
      mmv
      mnamer
      mosh
      mp3gain
      mpc_cli
      mpv
      msmtp
      musikcube
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
      nixopsUnstable
      nixpkgs-fmt
      nixpkgs-review
      nload
      nmap
      notmuch
      ntfs3g
      ntfsprogs
      obs-studio
      oneshot
      openssl
      p7zip
      pamixer
      pandoc
      paperkey
      papirus-icon-theme
      pastel
      pbzip2
      pciutils
      peep
      pigz
      pipr
      pixz
      podman
      podman-compose
      posix_man_pages
      powertop
      procs
      profanity
      pulseaudio
      pulsemixer
      pv
      pxz
      qemu
      qrencode
      radeontop
      recode
      reptyr
      restic
      ripgrep
      rq
      rsync
      rustscan
      safecopy
      screen
      sd
      shellcheck
      signal-desktop
      silicon
      silver-searcher
      smartmontools
      so
      sox
      spek
      ssdeep
      starship
      stow
      strace
      tasksh
      taskwarrior
      taskwarrior-tui
      tealdeer
      termdown
      termshark
      termusic
      testdisk
      tig
      toilet
      tokei
      tree
      treefmt
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
      viu
      vivid
      vlc
      vlock
      vorbis-tools
      vorbisgain
      vscodium
      vuizvui.devhell.vim
      w3m
      wavpack
      wget
      which
      wiki-tui
      wipe
      wireguard-tools
      wordgrinder
      writedisk
      wuzz
      xfsprogs
      xh
      xorg.xev
      xsv
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
