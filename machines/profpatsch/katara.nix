{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:
let

  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib; };

in {

  imports = [
    ./base-workstation.nix
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

    hardware.trackpoint = {
      speed = 280;
    };

    hardware.pulseaudio = {
      enable = true;
      zeroconf.discovery.enable = true;
      # for Pillars of Eternity
      support32Bit = true;
    };
    # steam
    hardware.opengl.driSupport32Bit = true;


    # needed by some games (TODO: general module for games)
    # hardware.opengl.driSupport32Bit = true;
    vuizvui.hardware.thinkpad.enable = true;

    ######
    # Nix

    nix.maxJobs = 4;
    # what was this activated for?!
    # vuizvui.enableGlobalNixpkgsConfig = true;

    ##########
    # Network

    networking.hostName = "katara";
    networking.supplicant.wlp3s0 = {
      configFile = {
        path = "/var/wifi-networks";
        writable = true;
      };
      userControlled.enable = true;
    };
    # networking.networkmanager.enable = true;

    # networking.bonds = {
    #   wifiAndEthernet = {
    #     interfaces = [ "wlp3s0" "enp0s25" ];
    #     driverOptions = {
    #       miimon = "100";
    #       primary = "enp0s25";
    #       mode = "active-backup";
    #     };
    #   };
    # };

    ###########
    # Packages

    environment.extraOutputsToInstall = [ "devdoc" ];
    environment.systemPackages = with pkgs;
    let
      systemPkgs =
      [
        atool               # archive tools
        gnupg gnupg1compat  # PGP encryption
        imagemagick         # image conversion
        jmtpfs              # MTP fuse
        mosh                # ssh with stable connections
        nfs-utils           # the filesystem of the future for 20 years
        sshfsFuse           # mount ssh machines
        tarsnap             # encrypting online backup tool
        # TODO move into atool deps
        unzip               # extract zip archives
        networkmanagerapplet
        wpa_supplicant_gui  # configure wireless connections
      ];
      xPkgs = [
        dmenu             # simple UI menu builder
        dunst             # notification daemon (interfaces with libnotify)
        alock             # lock screen
        libnotify         # notification library
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
        cabal2nix                    # convert cabal files to nixexprs
        # myPkgs.fast-init             # fast-init of haskell projects
        gitAndTools.git-annex        # version controlled binary file storage
        gitAndTools.git-dit          # decentral issue tracking for git

        httpie                       # nice http CLI
        # jid                          # interactive/incremental JSON digger
        # mercurial                    # the other version control system
        telnet                       # tcp debugging
        pkgs.vuizvui.profpatsch.nman # open man pages in temporary nix shell
        pkgs.vuizvui.profpatsch.warpspeed # trivial http file server
      ];
      documentation = [
        # mustache-spec NOT IN 16.09
      ];
      userPrograms = [
        abcde                # high-level cd-ripper with tag support
        anki mecab kakasi    # spaced repetition system & japanese analyzer
        # TODO integrate lame into audacity
        audacity lame.lib    # audio editor and mp3 codec
        myPkgs.beets         # audio file metadata tagger
        # chromium             # browser
        (chromium.override { enablePepperFlash = true; })
        pkgs.vuizvui.profpatsch.droopy # simple HTML upload server
        unfreeAndNonDistributablePkgs.dropbox-cli # dropbox.com client
        electrum             # bitcoin client
        emacs                # pretty neat operating system i guess
        feh                  # brother of meh, displays images in a meh way, but fast
        filezilla            # FTP GUI business-ready interface framework
        myPkgs.saneGhci      # <s>Glorious</s>Glasgow Haskell Compiler, mostly for ghci
        gimp                 # graphics
        gmpc                 # mpd client and best music player interface in the world
        haskellPackages.hledger # plain text accounting
        inkscape             # vector graphics
        libreoffice          # a giant ball of C++, that sometimes helps with proprietary shitformats
        lilyterm-git         # terminal emulator, best one around
        myPkgs.mpv           # you are my sun and my stars, and you play my stuff.
        newsbeuter           # RSS/Atom feed reader
        pass                 # standard unix password manager
        picard               # jean-luc, music tagger
        poppler_utils        # pdfto*
        ranger               # CLI file browser
        remind               # calender & reminder program
        rtorrent             # monster of a bittorrent client
        myPkgs.sent          # suckless presentation tool
        unfreeAndNonDistributablePkgs.steam # the one gaming platform
        myPkgs.xmpp-client   # CLI XMPP Client
        youtube-dl           # download videos
        zathura              # pdf viewer
      ];
      userScripts = with pkgs.vuizvui.profpatsch; [
        display-infos  # show time & battery
        show-qr-code   # display a QR code
        backlight      # adjust laptop backlight
      ];
      mailPkgs = [
        elinks               # command line browser
        mutt-with-sidebar    # has been sucking less since 1995
        msmtp                # SMTP client
        notmuch              # mail indexer
        pythonPackages.alot  # the next cool thing!
      ];
      nixPkgs = [
        nix-repl                  # nix REPL
        nix-prefetch-scripts      # prefetch store paths from various destinations
      ];
      tmpPkgs = [
        # TODO needs user service
        redshift   # increases screen warmth at night (so i don’t have to feel cold)
        # on remove keep pdfjam!
        (texlive.combine { inherit (texlive) scheme-medium latexmk IEEEtran needspace; })
        # myPkgs.nix-gen
      ];
    in systemPkgs ++ xPkgs ++ guiPkgs
    ++ programmingTools ++ documentation
    ++ userPrograms ++ userScripts
    ++ mailPkgs ++ nixPkgs ++ tmpPkgs;
    # system.extraDependencies = with pkgs; lib.singleton (
    #    # Haskell packages I want to keep around
    #    haskellPackages.ghcWithPackages (hpkgs: with hpkgs;
    #      [
    #        # frp
    #        frpnow
    #        gloss
    #        gtk
    #        frpnow-gtk
    #        frpnow-gloss

    #        lens
    #        wreq
    #        aeson-lens
    #      ]))
    #    ++
    #    # other packages that I use sometimes in a shell
    #    [
    #    ];

    ###########
    # Services

    services.searx = {
      enable = true;
      package = myPkgs.searx;
    };

    services.printing = {
      enable = true;
      gutenprint = true;
      # TODO
      # drivers = [ pkgs.cups-pdf ];
      # TODO
      # drivers = [ pkgs.foomatic_filters pkgs.foomatic-db-engine ];
    };

    # Automount
    services.udisks2.enable = true;

    services.logind.extraConfig = ''
      # want to be able to listen to music while laptop closed
      LidSwitchIgnoreInhibited=no
    '';

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

      synaptics = {
        enable = true;
        minSpeed = "0.6";
        maxSpeed = "1.5";
        accelFactor = "0.015";
        twoFingerScroll = true;
        vertEdgeScroll = false;
      };


      videoDrivers = [ "intel" ];

      displayManager = {
        sessionCommands = with pkgs; ''
            #TODO add as nixpkg
            export PATH+=":$HOME/scripts" #add utility scripts
            export EDITOR=emacsclient
            export TERMINAL=${lilyterm}/bin/lilyterm
            export TERM="$TERMINAL"

            ${xorg.xset}/bin/xset r rate 250 35

            set-background &
            # TODO xbindkeys user service file
            ${lib.getBin xbindkeys}/bin/xbindkeys
            nice -n19 dropbox-cli start &
            nm-applet &
            # synchronize clipboards
            ${lib.getBin autocutsel}/bin/autocutsel -s PRIMARY &
            ${lib.getBin twmn}/bin/twmnd &
          '';
      };

    };

    fonts.fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Source Code Pro" "DejaVu Sans Mono" ]; # TODO does not work
        sansSerif = [ "Liberation Sans" ];
      };
      ultimate = {
        enable = true;
        substitutions = "combi";
        preset = "ultimate4";
      };
    };
    fonts.fonts = with pkgs; [
      unfreeAndNonDistributablePkgs.corefonts
      source-han-sans-japanese
      source-han-sans-korean
      source-han-sans-simplified-chinese
      source-code-pro
      hasklig
      dejavu_fonts
      ubuntu_font_family
      league-of-moveable-type
      symbola # emoji
    ];

    ###########
    # Programs

    vuizvui.programs.gnupg = {
      enable = true;
      agent = {
        enable = true;
        sshSupport = true;
      };
    };

    # TODO: base config?
    vuizvui.programs.fish.fasd.enable = true;

    # build derivation on taalo
    vuizvui.user.aszlig.programs.taalo-build.enable = true;

    vuizvui.user.profpatsch.programs.scanning = {
      enable = true;
      remoteScanners = ''
        hannswurscht.openlab.lan
      '';
    };

    #######
    # Misc

    security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];

    ########
    # Fixes

    # fix for emacs ssh
    programs.bash.promptInit = "PS1=\"# \"";

    ################
    # User services
    systemd.user = lib.mkMerge [

      (lib.mkIf config.vuizvui.programs.gnupg.enable {
        services.unlock-password-store = {
          description = "unlock the user password store";
          wantedBy = [ "default.target" ];
          # make sure gpg-agent is running
          wants = [ "gpg-agent.service" ];
          after = [ "gpg-agent.service" ];
          serviceConfig = {
            # use special unlock key in the password store (needs to exist of course)
            ExecStart = "${lib.getBin pkgs.pass}/bin/pass misc/unlock";
            StandardOutput = "null";
          };
        };
        timers.unlock-password-store = {
          description = "unlock password store on system start";
          wantedBy = [ "timers.target" ];
          timerConfig.OnStartupSec = "5s";
        };
       })

      {
        services.mbsync = {
          description = "mbsync job";
          wants = [ "notmuch.service" ];
          before = [ "notmuch.service"];
          path = [ pkgs.pass ];
          serviceConfig = {
            Restart = "no";
            ExecStart = "${pkgs.isync}/bin/mbsync -a";
            };
        };
        timers.mbsync = {
          description = "run mbsync job every 15 minutes";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnStartupSec="10s";
            OnUnitActiveSec ="15m";
          };
        };
        services.notmuch = {
          description = "notmuch job";
          serviceConfig = {
            Restart = "no";
            ExecStart = "${pkgs.notmuch}/bin/notmuch new";
            };
        };
      }

    ];

  };
}
