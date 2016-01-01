{ config, pkgs, lib, ... }:
let

  myPkgs = import ./pkgs.nix { inherit pkgs; };

  mytexlive = with pkgs.texlive; combine { inherit scheme-medium minted units collection-bibtexextra; };

in {

  config = rec {

    #########
    # Kernel

    boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" ];
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    boot.loader.grub.device = "/dev/sda";
    boot.initrd.luks.devices = [ { device = "/dev/sda2"; name = "cryptroot"; } ];


    ###########
    # Hardware


    fileSystems."/" = {
      device = "/dev/dm-0";
      fsType = "btrfs";
    };

    fileSystems."/boot" = {
      device = "/dev/sda1";
      fsType = "ext3";
    };

    hardware.pulseaudio.enable = true;
    vuizvui.hardware.thinkpad.enable = true;


    ######
    # Nix

    nix.maxJobs = 2;
    vuizvui.enableGlobalNixpkgsConfig = true;

    ##########
    # Network

    networking.hostName = "katara";
    networking.networkmanager.enable = true;

    networking.firewall = {
      enable = true;
      # Programmer’s dilemma
      allowedTCPPortRanges = [
        { from = 8000; to = 8005; }
        { from = 8080; to = 8085; }
      ];
    };

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "us";
      defaultLocale = "en_US.UTF-8";
    };


    ###########
    # Packages

    environment.systemPackages = with pkgs;
    let
      systemPkgs = [
        atool             # archive tools
        curl              # transfer data to/from a URL
        dos2unix          # text file conversion
        file              # file information
        fish              # friendly user shell
        git               # version control system
        gnupg             # PGP encryption
        htop              # top replacement
        imagemagick       # image conversion
        jmtpfs            # MTP fuse
        gnumake           # make
        manpages          # system manpages (not included by default)
        mkpasswd          # UNIX password creator
        mosh              # ssh with stable connections
        nmap              # stats about clients in the network
        silver-searcher   # file content searcher, > ack > grep
        stow              # dotfile management
        tmux              # detachable terminal multiplexer
        traceroute        # trace ip routes
        vim               # slight improvement over vi
        wget              # the other URL file fetcher
      ];
      xPkgs = [
        dmenu             # simple UI menu builder
        dunst             # notification daemon (implements libnotify)
        i3lock            # lock screen
        libnotify         # notification library
        lxappearance      # GTK theme chooser
        xbindkeys         # keybinding manager
        myPkgs.taffybar          # status bar
      ];
      guiPkgs = [
        gnome3.adwaita-icon-theme
        # TODO: get themes to work. See notes.org.
        gnome3.gnome_themes_standard
        # kde4.oxygen-icons TODO
      ];
      userPrograms = [
        abcde                # high-level cd-ripper with tag support
        anki                 # spaced repetition system
        audacity lame        # audio editor and mp3 codec
        beets                # audio file metadata tagger
        chromium             # browser
        #(chromium.override { enablePepperFlash = true; })
        dropbox-cli          # dropbox.com client
        emacs                # pretty neat operating system i guess
        feh                  # brother of meh, displays images in a meh way, but fast
        filezilla            # FTP GUI business-ready interface framework
        # gajim                # XMPP client that still sucks, but not as hard as pidgin
        poezio               # XMPP client
        gimp                 # image editor
        gmpc                 # mpd client and best music player interface in the world
        inkscape             # vector graphics editor
        keepassx             # password manager
        libreoffice          # a giant ball of C++, that sometimes helps with proprietary shitformats
        lilyterm             # terminal emulator, best one around
        # lyx mytexlive      # you didn’t see a thing
        mpv                  # you are my sun and my stars. and you play my stuff.
        newsbeuter           # RSS/Atom feed reader
        networkmanagerapplet # NetworkManager status bar widget
        ranger               # command line file browser
        pkgs.vuizvui.show-qr-code # display a QR code
        zathura              # pdf viewer
      ];
      mailPkgs = [
        elinks             # command line browser
        myPkgs.offlineimap # IMAP client
        # TODO mutt-kz
        mutt-with-sidebar  # has been sucking less since 1970
        msmtp              # SMTP client
        notmuch            # mail indexer
      ];
      nixPkgs = [
        nix-repl                  # nix REPL
        nix-prefetch-scripts      # prefetch store paths from various destinations
        haskellPackages.cabal2nix # convert cabal files to nix
      ];
      tmpPkgs = [
        # needs user service
        redshift   # increases screen warmth at night (so i don’t have to feel cold)
      ];
    in systemPkgs ++ xPkgs ++ guiPkgs ++ userPrograms ++ nixPkgs ++ mailPkgs ++ nixPkgs ++ tmpPkgs;
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
         #wkhtmltopdf
         rustc
       ];

    ###########
    # Services

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Enable CUPS to print documents.
    services.printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };

    time.timeZone = "Europe/Berlin";

    # redshift TODO as user
    services.redshift = {
      # enable = true;
      latitude = "48";
      longitude = "10";
      temperature.day = 6300;
    };

    # locate
    services.locate = {
      enable = true;
    };

    # Automount
    services.udisks2.enable = true;

    services.journald.extraConfig = "SystemMaxUse=50M";

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
      desktopManager.xterm.enable = false;

      # TODO: include taffybar
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      displayManager = {
        desktopManagerHandlesLidAndPower = false;
        sessionCommands =
            ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export EDITOR=emacsclient
            xset r rate 250 35
            set-background &
            # TODO xbindkeys user service file
            xbindkeys
            nice -n19 dropbox start &
            nm-applet &
            gajim &
            '';
      };

      startGnuPGAgent = true;

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
      corefonts
      source-han-sans-japanese
      source-han-sans-korean
      source-han-sans-simplified-chinese
      source-code-pro
      dejavu_fonts
      ubuntu_font_family
    ];


    ########
    # Users

    # Nobody wants mutable state. :)
    users.mutableUsers = false;
    users.extraUsers = 
      let authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];
      in {
        philip = rec {
  	name = "philip";
  	group = "users";
          extraGroups = [ "wheel" "networkmanager" ];
  	uid = 1000;
  	createHome = true;
  	home = "/home/philip";
          passwordFile = "${home}/.config/passwd";
          # password = "test"; # in case of emergency, break glass
  	shell = "/run/current-system/sw/bin/bash";
          openssh.authorizedKeys.keys = authKeys;
      };
    };

    ###########
    # Programs

    # see gpgAgent
    programs.ssh.startAgent = false;

    #######
    # Misc

    # TODO seems to work only sometimes in chromium
    # security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];

    ########
    # Fixes

    # fix for emacs ssh
    programs.bash.promptInit = "PS1=\"# \"";

  };
}
