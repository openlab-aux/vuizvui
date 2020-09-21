{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:
let

  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib unfreeAndNonDistributablePkgs; };

  lock-screen = pkgs.writers.writeDashBin "lock-screen" ''
    set -e
    revert() {
      ${pkgs.xorg.xset}/bin/xset dpms 0 0 0
    }
    trap revert HUP INT TERM EXIT
    ${pkgs.xorg.xset}/bin/xset +dpms dpms 2 2 2
    ${pkgs.i3lock}/bin/i3lock \
      --nofork \
      --color=000000
  '';

in {

  imports = [
    ./base-workstation.nix
  ];

  config = {

    #########
    # Kernel

    boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" ];
    boot.loader.grub.device = "/dev/disk/by-id/ata-CT500MX500SSD1_1809E130BEE8";

    # VPN support
    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];

    boot.initrd.luks.devices.cryptroot.device = "/dev/disk/by-uuid/2e1c433f-4a54-4f04-9073-3639b66b975d";

    ###########
    # Hardware

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/5339f027-df78-437b-8a4c-39b93abc40b9";
      fsType = "btrfs";
      options = [ "ssd" "subvol=/katarafs" ];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/53042c4f-bbf2-418b-bf85-5d148ab5dda0";
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
      package = pkgs.pulseaudio.override {
        bluetoothSupport = true;
      };
      extraModules = [ pkgs.pulseaudio-modules-bt ];
    };
    # steam
    # needed by some games (TODO: general module for games)
    hardware.opengl.driSupport32Bit = true;

    # TODO: kinda broken?
    # i18n = {
    #   inputMethod = {
    #     enabled = "fcitx";
    #     Japanese input
    #     fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
    #   };
    # };
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    ######
    # Nix

    nix.maxJobs = 2;
    vuizvui.modifyNixPath = false;
    nix.nixPath = [
      "vuizvui=${myLib.philip.home}/vuizvui"
      "nixpkgs=${myLib.philip.home}/nixpkgs"
      # TODO: nicer?
      "nixos-config=${pkgs.writeText "shiki-configuration.nix" ''
        (import <vuizvui/machines>).profpatsch.shiki.config
      ''}"
    ];

    nix.distributedBuilds = true;
    nix.trustedUsers = [ "@wheel" ];
    nix.buildMachines = [
      # access to the nix-community aarch64 build box
      {
        hostName = "aarch64.nixos.community";
        maxJobs = 64;
        sshKey = "/root/aarch64-build-box/ssh-key";
        sshUser = "Profpatsch";
        system = "aarch64-linux";
        supportedFeatures = [ "big-parallel" ];
      }
      # tweag remote builder
      {
        hostName = "build01.tweag.io";
        maxJobs = 24;
        sshKey = "/root/.ssh/tweag-nix-builder";
        sshUser = "nix";
        system = "x86_64-linux";
        supportedFeatures = [ "big-parallel" ];
      }
    ];
    nix.extraOptions = ''
      builders-use-substitutes = true
      auto-optimise-store = true
    '';

    ##########
    # Network

    networking.hostName = "shiki";

    networking.networkmanager.enable = true;

    # TODO: bond eth and wifi again
    # networking.bonds = {
    #   wifiAndEthernet = {
    #     interfaces = [ "wlp3s0" "enp0s25" ];
    #     driverOptions = {
    #       # how often to check for link failures, i.e. ethernet down (ms)
    #       miimon = "500";
    #       primary = "enp0s25";
    #       primary_reselect = "always";
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
        atool                # archive tools
        gnupg gnupg1compat   # PGP encryption
        imagemagick          # image conversion
        jmtpfs               # MTP fuse
        mosh                 # ssh with stable connections
        sshfsFuse            # mount ssh machines
        # TODO move into atool deps
        unzip                # extract zip archives
        networkmanagerapplet # for nm-connection-editor
      ];
      xPkgs = [
        lock-screen       # lock screen
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
      programmingTools = [
        cabal2nix                    # convert cabal files to nixexprs
        # myPkgs.fast-init             # fast-init of haskell projects
        # gitAndTools.git-annex        # version controlled binary file storage
        # gitAndTools.git-dit          # decentral issue tracking for git
        # gitAndTools.git-hub          # lightweight GitHub integration

        # TODO: move to user config
        direnv
        httpie                       # nice http CLI
        jq                           # json filter
        telnet                       # tcp debugging
        # TODO: make static binaries
        pkgs.vuizvui.profpatsch.nix-http-serve # serve nix builds and rebuild on reloads
        pkgs.vuizvui.profpatsch.nman # open man pages in temporary nix shell
        pkgs.vuizvui.profpatsch.warpspeed    # trivial http file server
        # pkgs.vuizvui.profpatsch.nix-gen      # generate nix expressions
        pkgs.vuizvui.profpatsch.watch-server # restart server on code change
        pkgs.vuizvui.profpatsch.until        # restart until cmd succeeds
        execline
        pkgs.vuizvui.profpatsch.dhall
        pkgs.vuizvui.profpatsch.dhall-flycheck
      ];
      documentation = [
        # mustache-spec NOT IN 16.09
      ];
      userPrograms = [
        # abcde                # high-level cd-ripper with tag support
        anki mecab kakasi    # spaced repetition system & japanese analyzer
        # TODO integrate lame into audacity
        audacity lame.lib    # audio editor and mp3 codec
        # myPkgs.beets         # audio file metadata tagger
        firefox              # browser
        chromium             # second browser for good measure
        cups                 # print tools, mainly for lp(1)
        pkgs.vuizvui.profpatsch.droopy # simple HTML upload server
        # electrum             # bitcoin client
        emacs                # pretty neat operating system i guess
        imv                  # young brother of feh and less meh
        filezilla            # FTP GUI business-ready interface framework
        gimp                 # graphics
        inkscape             # vector graphics
        libreoffice          # a giant ball of C++, that sometimes helps with proprietary shitformats
        myPkgs.mpv           # you are my sun and my stars, and you play my stuff.
        pass                 # standard unix password manager
        picard               # jean-luc, music tagger
        poppler_utils        # pdfto*
        ranger               # CLI file browser
        remind               # calender & reminder program
        taskwarrior tasksh   # task manager
        unfreeAndNonDistributablePkgs.steam # the one gaming platform
        youtube-dl           # download videos
        zathura              # pdf viewer
      ];
      userScripts = with pkgs.vuizvui.profpatsch;
        let
          di-notify = pkgs.vuizvui.profpatsch.writeExeclineBin "display-infos-notify" {} [
            "backtick" "-i" "DI" [ "${display-infos}/bin/display-infos" ]
            "importas" "DI" "DI"
            "${pkgs.libnotify}/bin/notify-send" "$DI"
          ];
        in [
        display-infos  # show time & battery
        di-notify      # same, but pipe to libnotify
        show-qr-code   # display a QR code
        backlight      # adjust laptop backlight
        sfttime        # geek time
      ];
      mailPkgs = [
        elinks               # command line browser
        msmtp                # SMTP client
        mu                   # mail indexing w/ emacs mode
      ];
      nixPkgs = [
        # nix-diff                  # structurally diff two derivations
        nix-prefetch-scripts      # prefetch store paths from various destinations
        pkgs.vuizvui.taalo-build  # build derivation on taalo
      ];
      tmpPkgs = [
        # TODO needs user service
        redshift   # increases screen warmth at night (so i don’t have to feel cold)
        # pdfjam is the best CLI pdf modification suite
        (texlive.combine { inherit (texlive) scheme-small pdfjam; })
        # move script/nix-cache-binary to here
        cdb
        myPkgs.zoomboxed
      ];
    in systemPkgs ++ xPkgs ++ guiPkgs
    ++ programmingTools ++ documentation
    ++ userPrograms ++ userScripts
    ++ mailPkgs ++ nixPkgs ++ tmpPkgs;

    ###########
    # Services


    # Automount
    services.udisks2.enable = true;

    services.logind.extraConfig = ''
      # want to be able to listen to music while laptop closed
      LidSwitchIgnoreInhibited=no
    '';

    # TMP

    vuizvui.services.guix.enable = true;
    ###################
    # Graphical System

    services.xserver = {
      videoDrivers = [ "intel" ];
    };

    fonts = {
      fonts = [
        unfreeAndNonDistributablePkgs.corefonts
        pkgs.source-han-sans-japanese
        pkgs.source-han-sans-korean
        pkgs.source-han-sans-simplified-chinese
        pkgs.source-code-pro
        pkgs.hasklig
        pkgs.dejavu_fonts
        pkgs.ubuntu_font_family
        pkgs.league-of-moveable-type
        pkgs.noto-fonts-emoji
        # pkgs.zbalermorna
      ];

      enableDefaultFonts = true;
      fontconfig = {
        enable = true;
        defaultFonts = {
          monospace = [ "Source Code Pro" ];
          serif = [ "Liberation Serif" ];
          sansSerif = [ "Liberation Sans" ];
          emoji = [ "Noto Color Emoji" "Noto Emoji" ];
        };
      };
    };

    services.printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.gutenprintBin pkgs.hplip ];
    };

    ###########
    # Programs

    vuizvui.programs.gnupg = {
      enable = true;
      agent = {
        enable = true;
        sshSupport = true;
      };
    };

    vuizvui.user.profpatsch.programs.scanning = {
      enable = true;
      #remoteScanners = ''
      #  hippie.lab
      #'';
    };

    # virtualisation.docker.enable = true;

    #######
    # Misc

    security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];

    ########
    # Fixes

    # fix for emacs ssh
    programs.bash.promptInit = "PS1=\"# \"";

    ################
    # User services
    vuizvui.services.profpatsch.dunst = {
      enable = true;
      settings = {
        global = {
          font = "Sans 18";
          markup = "full";
          padding = 3;
          horizontal_padding = 3;
        };
      };
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita";
      };
    };

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
          # run ~five seconds after user logs in
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
        services.mu = {
          description = "mu job";
          serviceConfig = {
            Restart = "no";
            ExecStart = "${pkgs.notmuch}/bin/notmuch new";
            };
        };
      }


      ({
      #   services.pyrnotify-ssh-connection = {
      #     description = "ssh connection to make pyrnotify work";
      #     serviceConfig = {
      #       # TODO: get out of the gpg-agent service directly
      #       Environment = ''"SSH_AUTH_SOCK=%t/gnupg/S.gpg-agent.ssh"'';
      #       ExecStart = pkgs.writeScript "pyrnotify-start-ssh" ''
      #         #!${pkgs.stdenv.shell}
      #         set -e
      #         # first delete the socket file if it exists
      #         # otherwise the forward doesn’t work
      #         ${lib.getBin pkgs.openssh}/bin/ssh \
      #           bigmac \
      #           "rm /home/bigmac/.weechat/pyrnotify.socket"
      #         # forwards the remote socket over ssh
      #         # thE options make it disconnect after 45 sec
      #         # by sending a keepalive packet every 15 seconds
      #         # and retrying 3 times
      #         ${lib.getBin pkgs.openssh}/bin/ssh \
      #           -o ServerAliveInterval=15 \
      #           -o ServerAliveCountMax=3 \
      #           -o ExitOnForwardFailure=yes \
      #           -R /home/bigmac/.weechat/pyrnotify.socket:localhost:8099 \
      #           -N \
      #           bigmac
      #       '';
      #     };
      #     requires = [ "gpg-agent.service" ];
      #     after = [ "gpg-agent.service" ];
      #   };
      #   services.pyrnotify-listen = rec {
      #     description = "get notified about weechat messages";
      #     serviceConfig = {
      #       ExecStart = "${lib.getBin pkgs.python
      #         }/bin/python ${myPkgs.pyrnotify} 8099";
      #       Restart = "on-failure";
      #       RestartSec = "5s";
      #     };
      #     bindsTo = [ "pyrnotify-ssh-connection.service" ];
      #     after = [ "pyrnotify-ssh-connection.service" ];
      #     wantedBy = [ "default.target" ];
      #   };
      })

    ];

  };
}
