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
      #mnamer
      #nixops
      #pipr
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
      element-desktop
      ethtool
      fd
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
      gh
      ghostscript
      ghostwriter
      git
      git-trim
      gitinspector
      gitui
      glow
      gnome.adwaita-icon-theme
      gnome.defaultIconTheme
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
      handlr
      hdparm
      hexedit
      hexyl
      hicolor-icon-theme
      hplipWithPlugin
      htop
      imagemagick
      imv
      ipcalc
      iprange
      iptraf-ng
      ipv6calc
      jfsutils
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
      manpages
      mapcidr
      mdbook
      mdbook-mermaid
      mediainfo
      menyoki
      mkvtoolnix
      mmv
      monkeysAudio
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
      nixpkgs-fmt
      nload
      nmap
      notmuch
      ntfs3g
      ntfsprogs
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
      ripgrep
      rq
      rsync
      rustscan
      safecopy
      screen
      sd
      signal-desktop
      silicon
      smartmontools
      smos
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
      telnet
      termdown
      termshark
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
      vivid
      vlc
      vlock
      vorbisTools
      vorbisgain
      vscodium
      vuizvui.devhell.vim
      w3m
      watchexec
      wavpack
      wget
      which
      wiki-tui
      wipe
      wireguard
      wordgrinder
      wuzz
      xfsprogs
      xh
      xlibs.xev
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
