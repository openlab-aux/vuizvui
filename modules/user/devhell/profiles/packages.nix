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
      #dogdns
      #nixopsUnstable
      #onefetch
      #wordgrinder
      #wuzz
      abook
      accountsservice
      alacritty
      apg
      aria2
      ascii
      asn
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
      dos2unix
      dstask
      du-dust
      dua
      duf
      e2fsprogs
      easyeffects
      ethtool
      eva
      exfat
      fastfetch
      fd
      feh
      fend
      ffmpeg-full
      ffsend
      figlet
      file
      firefox
      flac
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
      glab
      glow
      gnome.adwaita-icon-theme
      gnufdisk
      gnumake
      gnupg
      gomuks
      gopass
      gpg-tui
      gpgme
      gptfdisk
      graphviz
      grex
      gurk-rs
      handlr
      hdparm
      hexedit
      hexyl
      hicolor-icon-theme
      hplipWithPlugin
      htop
      hydra-check
      imagemagick
      inetutils
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      jql
      kmon
      lemmeknow
      lfs
      lftp
      libarchive
      libreoffice
      lm_sensors
      lsd
      lsof
      lxd
      macchina
      maim
      mako
      man-pages
      man-pages-posix
      mapcidr
      mdbook
      mdbook-mermaid
      mediainfo
      menyoki
      miniserve
      mkvtoolnix
      mmv
      mnamer
      mosh
      mp3gain
      mpc_cli
      mpv
      msmtp
      navi
      ncdu
      ncmpcpp
      neomutt
      nethack
      nethogs
      netrw
      netsniff-ng
      nix-output-monitor
      nix-update
      nixfmt
      nixpkgs-fmt
      nixpkgs-review
      nload
      nmap
      notmuch
      ntfs3g
      ntfsprogs
      oculante
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
      rclone
      recode
      reptyr
      restic
      ripgrep
      rq
      rsync
      rustic-rs
      safecopy
      screen
      sd
      shellcheck
      shellharden
      signal-desktop
      silicon
      silver-searcher
      smartmontools
      so
      sox
      speedtest-rs
      spek
      ssdeep
      starship
      steam-tui
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
      tuifeed
      tut
      ugrep
      units
      unrar
      unzip
      urlscan
      usbutils
      vanilla-dmz
      virt-manager
      virt-viewer
      visidata
      viu
      vivid
      vlock
      vorbis-tools
      vorbisgain
      vscodium
      vuizvui.devhell.nvim
      vuizvui.devhell.vim
      w3m
      wavpack
      wget
      which
      whois
      wiki-tui
      wipe
      wireguard-tools
      writedisk
      wtf
      xcp
      xdg-utils
      xfsprogs
      xh
      xsv
      yt-dlp
      zathura
      zbar
      zellij
      zip
      zotero
      zstd
    ];
  };
}
