{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:
let

  myPkgs = import ./pkgs.nix { inherit pkgs lib; };

in {

  imports = [
    ./base.nix
  ];

  config = rec {

    #########
    # Kernel

    boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" ];
    boot.loader.grub.device = "/dev/sda";
    boot.initrd.luks.devices = [ { device = "/dev/sda2"; name = "cryptroot"; } ];

    ###########
    # Hardware

    fileSystems."/" = {
      device = "/dev/dm-0";
      fsType = "btrfs";
      options = [ "ssd" ];
    };

    fileSystems."/boot" = {
      device = "/dev/sda1";
      fsType = "ext3";
    };

    hardware.pulseaudio = {
      enable = true;
      # package = pkgs.pulseaudioFull;
      # zeroconf.discovery = true;
      # zeroconf.publish = true;
    };

    vuizvui.hardware.thinkpad.enable = true;

    ######
    # Nix

    nix.maxJobs = 2;
    # what was this activated for?!
    # vuizvui.enableGlobalNixpkgsConfig = true;

    ##########
    # Network

    networking.hostName = "katara";

    networking.networkmanager.basePackages =
      with pkgs; {
        # the openssl backend doesn’t like the protocols of my university
        networkmanager_openconnect =
          pkgs.networkmanager_openconnect.override { openconnect = pkgs.openconnect_gnutls; };
        inherit networkmanager modemmanager wpa_supplicant
                networkmanager_openvpn networkmanager_vpnc
                networkmanager_pptp networkmanager_l2tp;
    };


    ###########
    # Packages

    environment.systemPackages = with pkgs;
    let
      systemPkgs =
      [
        atool             # archive tools
        gnupg gnupg1compat # PGP encryption
        imagemagick       # image conversion
        jmtpfs            # MTP fuse
        mosh              # ssh with stable connections
        nfs-utils         # the filesystem of the future for 20 years
        # TODO move into atool deps
        unzip             # extract zip archives
      ];
      xPkgs = [
        dmenu             # simple UI menu builder
        dunst             # notification daemon (interfaces with libnotify)
        alock             # lock screen
        libnotify         # notification library
        myPkgs.taffybar   # status bar
        xclip             # clipboard thingy
        xorg.xkill        # X11 application kill
      ];
      guiPkgs = [
        gnome3.adwaita-icon-theme
        # TODO: get themes to work. See notes.org.
        gnome3.gnome_themes_standard
        pavucontrol
        networkmanagerapplet
      ];
      hp = haskellPackages;
      programmingTools = [
        hp.cabal2nix          # convert cabal files to nixexprs
        myPkgs.git-annex # version controlled binary file storage
        # mercurial             # the other version control system
        telnet                # tcp debugging
      ];
      userPrograms = [
        abcde                # high-level cd-ripper with tag support
        anki                 # spaced repetition system
        # TODO integrate lame into audacity
        audacity lame.lib    # audio editor and mp3 codec
        myPkgs.beets         # audio file metadata tagger
        chromium             # browser
        # (chromium.override { enablePepperFlash = true; })
        # droopy               # simple HTML upload server
        unfreeAndNonDistributablePkgs.dropbox-cli # dropbox.com client
        emacs                # pretty neat operating system i guess
        feh                  # brother of meh, displays images in a meh way, but fast
        filezilla            # FTP GUI business-ready interface framework
        ghc                  # <s>Glorious</s>Glasgow Haskell Compiler, mostly for ghci
        gimp                 # graphics
        gmpc                 # mpd client and best music player interface in the world
        httpie               # nice http CLI
        inkscape             # vector graphics
        libreoffice          # a giant ball of C++, that sometimes helps with proprietary shitformats
        lilyterm             # terminal emulator, best one around
        myPkgs.mpv           # you are my sun and my stars. and you play my stuff.
        newsbeuter           # RSS/Atom feed reader
        pass                 # standard unix password manager
        myPkgs.poezio               # CLI XMPP client
        poppler_utils        # pdfto*
        ranger               # CLI file browser
        remind               # calender & reminder program
        rtorrent             # monster of a bittorrent client
        myPkgs.sent          # suckless presentation tool
        pkgs.vuizvui.show-qr-code # display a QR code
        zathura              # pdf viewer
      ];
      mailPkgs = [
        elinks             # command line browser
        myPkgs.offlineimap # IMAP client
        mutt-with-sidebar  # has been sucking less since 1970
        msmtp              # SMTP client
        notmuch            # mail indexer
        pythonPackages.alot # the next cool thing!
      ];
      nixPkgs = [
        nix-repl                  # nix REPL
        nix-prefetch-scripts      # prefetch store paths from various destinations
        # haskellPackages.cabal2nix # convert cabal files to nix
      ];
      tmpPkgs = [
        # TODO needs user service
        redshift   # increases screen warmth at night (so i don’t have to feel cold)
      ];
    in systemPkgs ++ xPkgs ++ guiPkgs ++ programmingTools ++ userPrograms ++ mailPkgs ++ nixPkgs ++ tmpPkgs;
    system.extraDependencies = with pkgs; lib.singleton (
       # Haskell packages I want to keep around
       haskellPackages.ghcWithPackages (hpkgs: with hpkgs;
         [
           # frp
           frpnow
           gloss
           gtk
           frpnow-gtk
           frpnow-gloss

           lens
           wreq
           aeson-lens
         ]))
       ++
       # other packages that I use sometimes in a shell
       [
       ];

    ###########
    # Services

    services.printing = {
      enable = true;
      gutenprint = true;
      # TODO
      # drivers = [ pkgs.cups-pdf ];
      # TODO
      # drivers = [ pkgs.foomatic_filters pkgs.foomatic-db-engine ];
    };

    # redshift TODO as user
    services.redshift = {
      # enable = true;
      latitude = "48";
      longitude = "10";
      temperature.day = 6300;
    };

    # Automount
    services.udisks2.enable = true;

    # TODO: taffybar battery depends on this
    services.upower.enable = true;

    ###################
    # Graphical System

    services.xserver = {
      enable = true;
      layout = "de";
      xkbVariant = "neo";
      xkbOptions = "altwin:swap_alt_win";
      serverFlagsSection = ''
        Option "StandbyTime" "10"
        Option "SuspendTime" "20"
        Option "OffTime" "30"
      '';
      synaptics.enable = true;
      synaptics.minSpeed = "0.5";
      synaptics.accelFactor = "0.01";
      videoDrivers = [ "intel" ];

      # otherwise xterm is enabled, creating an xterm that spawns the window manager.
      # TODO: Try to fix (annoying for new users)
      desktopManager.xterm.enable = false;

      # TODO: include taffybar
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      # autorepeat = {
      #   enable = true;
      #   delay = 250;
      #   rate = 35;
      # };

      displayManager = {
        sessionCommands = with pkgs;
            ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export PATH+=":$HOME/.bin" #add (temporary) executables
            export EDITOR=emacsclient

            ${gnupg}/bin/gpg-connect-agent /bye
            unset SSH_AGENT_PID
            export SSH_AUTH_SOCK="''${HOME}/.gnupg/S.gpg-agent.ssh"

            ${xorg.xset}/bin/xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            ${xbindkeys}/bin/xbindkeys
            nice -n19 dropbox-cli start &
            ${networkmanagerapplet}/bin/nm-applet &
            # synchronize clipboards
            ${autocutsel}/bin/autocutsel -s PRIMARY &
            '';
      };

    };

    fonts.fontconfig = {
      defaultFonts = {
        monospace = [ "Source Code Pro" "DejaVu Sans Mono" ]; # TODO does not work
        sansSerif = [ "Liberation Sans" ];
      };
      # use overkill infinality settings from old Arch installation
      ultimate = {
        rendering = {
          INFINALITY_FT_FILTER_PARAMS = "08 24 36 24 08";
          INFINALITY_FT_FRINGE_FILTER_STRENGTH = "25";
          INFINALITY_FT_USE_VARIOUS_TWEAKS = "true";
          INFINALITY_FT_WINDOWS_STYLE_SHARPENING_STRENGTH = "25";
          INFINALITY_FT_STEM_ALIGNMENT_STRENGTH = "15";
          INFINALITY_FT_STEM_FITTING_STRENGTH = "15";
        };
      };
    };
    fonts.fonts = with pkgs; [
      unfreeAndNonDistributablePkgs.corefonts
      source-han-sans-japanese
      source-han-sans-korean
      source-han-sans-simplified-chinese
      source-code-pro
      dejavu_fonts
      ubuntu_font_family
      league-of-moveable-type
    ];


    ###########
    # Programs

      # gpg-agent; TODO: move to module
    programs.fish.shellInit = ''
        set -x GPG_TTY (tty)
      '';

    # TODO: base config?
    vuizvui.programs.fish.fasd.enable = true;

    # build derivation on taalo
    vuizvui.user.aszlig.programs.taalo-build.enable = true;

    vuizvui.user.profpatsch.programs.scanning.enable = true;

    #######
    # Misc

    security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];

    ########
    # Fixes

    # fix for emacs ssh
    programs.bash.promptInit = "PS1=\"# \"";

  };
}
