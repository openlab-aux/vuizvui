{ config, pkgs, lib, ... }:
let

  # TODO
  tmpAllowUnfree = drv:
    let n = import ../../nixpkgs-path.nix;
    in (import n { config = { allowUnfree = true; }; })."${drv}";


  myPkgs = import ./pkgs.nix { inherit pkgs lib; };
  fish = pkgs.fish;

  # mytexlive = with pkgs.texlive; combine { inherit minted; }; # inherit scheme-medium minted units collection-bibtexextra; };

  authKeys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhthfk38lzDvoI7lPqRneI0yBpZEhLDGRBpcXzpPSu+V0YlgrDix5fHhBl+EKfw4aeQNvQNuAky3pDtX+BDK1b7idbz9ZMCExy2a1kBKDVJz/onLSQxiiZMuHlAljVj9iU4uoTOxX3vB85Ok9aZtMP1rByRIWR9e81/km4HdfZTCjFVRLWfvo0s29H7l0fnbG9bb2E6kydlvjnXJnZFXX+KUM16X11lK53ilPdPJdm87VtxeSKZ7GOiBz6q7FHzEd2Zc3CnzgupQiXGSblXrlN22IY3IWfm5S/8RTeQbMLVoH0TncgCeenXH7FU/sXD79ypqQV/WaVVDYMOirsnh/ philip@nyx"];
 
  philip = rec {
    name = "philip";
    group = "users";
          extraGroups = [ "wheel" "networkmanager" "sambashare" ];
    uid = 1000;
    createHome = true;
    home = "/home/philip";
          passwordFile = "${home}/.config/passwd";
          # password = "test"; # in case of emergency, break glass
    shell = "${fish}/bin/fish";
          openssh.authorizedKeys.keys = authKeys;
    };

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
      options = [ "ssd" ];
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
    # what was this activated for?!
    # vuizvui.enableGlobalNixpkgsConfig = true;

    ##########
    # Network

    networking.hostName = "katara";
    networking.networkmanager = {
      enable = true;
      basePackages = with pkgs; {
        # the openssl backend doesn’t like the protocols of my university
        networkmanager_openconnect =
          pkgs.networkmanager_openconnect.override { openconnect = pkgs.openconnect_gnutls; };
        inherit networkmanager modemmanager wpa_supplicant
                networkmanager_openvpn networkmanager_vpnc
                networkmanager_pptp networkmanager_l2tp;
      };
    };

    networking.firewall.enable = false;

    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "us";
      defaultLocale = "en_US.UTF-8";
    };


    ###########
    # Packages


    environment.sessionVariables = { EDITOR = "${pkgs.vim}/bin/vim"; };

    environment.systemPackages = with pkgs;
    let
      systemPkgs =
      [
        atool             # archive tools
        curl              # transfer data to/from a URL
        dos2unix          # text file conversion
        fdupes            # file duplicate finder
        file              # file information
        gnupg gnupg1compat # PGP encryption
        htop              # top replacement
        imagemagick       # image conversion
        jmtpfs            # MTP fuse
        manpages          # system manpages (not included by default)
        mkpasswd          # UNIX password creator
        mosh              # ssh with stable connections
        nfs-utils         # the filesystem of the future for 20 years
        nmap              # stats about clients in the network
        smartmontools     # check disk state
        stow              # dotfile management
        tmux              # detachable terminal multiplexer
        traceroute        # trace ip routes
        # TODO move into atool deps
        unzip             # extract zip archives
        vim               # slight improvement over vi
        wget              # the other URL file fetcher
        wirelesstools     # iwlist (wifi scan)
      ];
      xPkgs = [
        dmenu             # simple UI menu builder
        dunst             # notification daemon (interfaces with libnotify)
        alock             # lock screen
        libnotify         # notification library
        myPkgs.taffybar   # status bar
        xbindkeys         # keybinding manager
        xclip             # clipboard thingy
        xorg.xkill        # X11 application kill
      ];
      guiPkgs = [
        gnome3.adwaita-icon-theme
        # TODO: get themes to work. See notes.org.
        gnome3.gnome_themes_standard
        pavucontrol
      ];
      hp = haskellPackages;
      programmingTools = [
        hp.cabal2nix          # convert cabal files to nixexprs
        git                   # version control system
        myPkgs.git-annex # version controlled binary file storage
        # mercurial             # the other version control system
        silver-searcher       # file content searcher, > ack > grep
        telnet                # tcp debugging
      ];
      userPrograms = [
        abcde                # high-level cd-ripper with tag support
        anki                 # spaced repetition system
        audacity lame        # audio editor and mp3 codec
        myPkgs.beets         # audio file metadata tagger
        # chromium             # browser
        (chromium.override { enablePepperFlash = true; })
        # droopy               # simple HTML upload server
        (tmpAllowUnfree "dropbox-cli")          # dropbox.com client
        emacs                # pretty neat operating system i guess
        feh                  # brother of meh, displays images in a meh way, but fast
        filezilla            # FTP GUI business-ready interface framework
        ghc                  # <s>Glorious</s>Glasgow Haskell Compiler, mostly for ghci
        gimp                 # graphics
        gmpc                 # mpd client and best music player interface in the world
        httpie               # nice http CLI
        libreoffice          # a giant ball of C++, that sometimes helps with proprietary shitformats
        lilyterm             # terminal emulator, best one around
        myPkgs.mpv           # you are my sun and my stars. and you play my stuff.
        newsbeuter           # RSS/Atom feed reader
        networkmanagerapplet # NetworkManager status bar widget
        pass                 # standard unix password manager
        poezio               # CLI XMPP client
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

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Enable CUPS to print documents.
    services.printing = {
      enable = true;
      gutenprint = true;
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
        sessionCommands =
            ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export PATH+=":$HOME/.bin" #add (temporary) executables
            export EDITOR=emacsclient

            gpg-connect-agent /bye
            unset SSH_AGENT_PID
            export SSH_AUTH_SOCK="''${HOME}/.gnupg/S.gpg-agent.ssh"

            xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            xbindkeys
            nice -n19 dropbox-cli start &
            nm-applet &
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
      corefonts
      source-han-sans-japanese
      source-han-sans-korean
      source-han-sans-simplified-chinese
      source-code-pro
      dejavu_fonts
      ubuntu_font_family
      league-of-moveable-type
    ];


    ########
    # Users

    # Nobody wants mutable state. :)
    users.mutableUsers = false;
    users.users = { inherit philip; };

    ###########
    # Programs

    # use gpg-agent
    programs.ssh.startAgent = false;

    # friendly user shell
    programs.fish = {
      enable = true;
      # gpg-agent; TODO: move to module
      shellInit = ''
        set -x GPG_TTY (tty)
      '';
    };
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
