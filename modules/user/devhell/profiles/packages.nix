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
      #dcfldd
      #dogdns
      #freerdpUnstable
      #gomuks
      #maim
      #mkvtoolnix
      #mnamer
      #netsniff-ng
      #nixopsUnstable
      #onefetch
      #oterm
      #rustdesk
      #spek
      #visidata
      #wordgrinder
      #wtfutil
      #wuzz
      abook
      accountsservice
      adwaita-icon-theme
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
      bc
      bcal
      beets
      binutils
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
      codeberg-cli
      colmena
      cryptsetup
      csvlens
      cuetools
      curlie
      cyme
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
      filen-cli
      firefox
      flac
      fortune
      fzf
      gammastep
      gcc
      gdb
      gh
      ghostscript
      git
      git-trim
      gitui
      glab
      glow
      gnumake
      gnupg
      gopass
      gpg-tui
      gpgme
      gptfdisk
      graphviz
      grex
      grim
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
      jq
      jql
      kanshi
      kmon
      lemmeknow
      lfs
      lftp
      libarchive
      libreoffice
      lm_sensors
      lsd
      lsof
      macchina
      mako
      man-pages
      man-pages-posix
      mapcidr
      mdbook
      mdbook-admonish
      mdbook-mermaid
      mediainfo
      menyoki
      miniserve
      mmv
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
      nix-output-monitor
      nix-update
      nixfmt-rfc-style
      nixpkgs-fmt
      nixpkgs-review
      nload
      nmap
      notmuch
      ntfs3g
      ntfsprogs
      nvtopPackages.amd
      oculante
      onefetch
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
      rofi
      rq
      rsync
      rustcat
      rustic-rs
      rustmission
      rwpspread
      safecopy
      screen
      sd
      shellcheck
      shellharden
      signal-desktop-bin
      silicon
      silver-searcher
      slurp
      smartmontools
      so
      sox
      speedtest-rs
      ssdeep
      starship
      steam-tui
      stow
      strace
      swaybg
      swayidle
      swaylock-effects
      taskwarrior-tui
      taskwarrior3
      tealdeer
      termdown
      termshark
      termusic
      testdisk
      tig
      tinty
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
      vale
      vanilla-dmz
      virt-manager
      virt-viewer
      viu
      vlock
      vorbis-tools
      vorbisgain
      vscodium
      vuizvui.devhell.nvim
      vuizvui.devhell.vim
      w3m
      wavpack
      waybar
      wf-recorder
      wget
      which
      whois
      wiki-tui
      wipe
      wireguard-tools
      wiremix
      wl-clipboard
      wl-mirror
      writedisk
      wthrr
      wttrbar
      xan
      xcp
      xdg-utils
      xfsprogs
      xh
      xwayland-satellite
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
