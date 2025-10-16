{ config, pkgs, unfreeAndNonDistributablePkgs, lib, ... }:
let

  myLib  = import ./lib.nix  { inherit pkgs lib; };
  myPkgs = import ./pkgs.nix { inherit pkgs lib myLib unfreeAndNonDistributablePkgs; };

  # magic constants that might need to be changed when migrating laptop models
  laptop = {
    cameraDevice = "/dev/video0";
  };

  tailscaleInterface = "tailscale0";

  lock-screen = pkgs.writers.writeDashBin "lock-screen" ''

    set -e
    revert() {
      # never turn off the screen (disable dpms)
      ${pkgs.xorg.xset}/bin/xset dpms 0 0 0
    }
    trap revert HUP INT TERM EXIT
    # turn off the screen after 5 seconds of inactivity
    ${pkgs.xorg.xset}/bin/xset +dpms dpms 5 5 5

    # A little dark maroon-ish hue so it’s easy to see when the screen lock is active
    ${pkgs.i3lock}/bin/i3lock \
      --nofork \
      --color=300000
  '';

in {

  imports = [
    ./base-workstation.nix
    (import ./profpatsch-thinkpad.nix { cpuType = "amd"; })
  ];

  config = {

    system.stateVersion = "23.11";

    system.autoUpgrade = {
      enable = true;
      dates = "18:00";
      channel = "https://nixos.org/channels/nixos-23.11";
      allowReboot = false;
    };

    #########
    # Kernel

    boot.initrd.luks.devices.cryptroot.device = "/dev/disk/by-label/root";

    # enable video loopback devices
    # via https://gist.github.com/TheSirC/93130f70cc280cdcdff89faf8d4e98ab
    boot.extraModulePackages = [
       config.boot.kernelPackages.v4l2loopback
    ];
    boot.kernelModules = [
      "v4l2loopback"
    ];
    boot.extraModprobeConfig = ''
      options v4l2loopback exclusive_caps=1
    '';
    # make sure /boot does not run out of space
    boot.loader.systemd-boot.configurationLimit = 20;

    # enable us to emulate aarch64-linux builds with nix-build
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

    ###########
    # Hardware

    fileSystems."/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
      options = [ "ssd" "subvol=/root" ];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-label/BOOT";
      fsType = "vfat";
      options = ["nofail"];
    };

    zramSwap.enable = true;

    hardware.trackpoint = {
      speed = 280;
    };

    powerManagement.cpuFreqGovernor = "powersave";

    # vuizvui.hardware.thinkpad.powerManagement = "auto-cpufreq";

    vuizvui.hardware.tolino.enable = true;

    services.udev.extraRules = ''
      # ATMEL ATm32U4DFU
      SUBSYSTEM=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", MODE="664", TAG+="uaccess"
    '';

    # hardware.pulseaudio = {
    #   enable = true;
    #   zeroconf.discovery.enable = true;
    #   # for Pillars of Eternity
    #   support32Bit = true;
    #   package = pkgs.pulseaudio.override {
    #     bluetoothSupport = true;
    #     jackaudioSupport = true;
    #   };
    #   extraModules = [ pkgs.pulseaudio-modules-bt ];
    # };
    # services.jack = {
    #   jackd.enable = true;
    #   alsa.enable = false;
    #   loopback.enable = true;
    # };
    services.pipewire = {
      enable = true;
      jack.enable = true;
      pulse.enable = true;
    };

    # needed by steam to run
    programs.steam.enable = true;
    # hardware.opengl = {
    #   enable = true;
    #   driSupport = true;
    #   driSupport32Bit = true;
    #   extraPackages = [
    #     pkgs.intel-media-driver
    #     (pkgs.intel-vaapi-driver.override { enableHybridCodec = true; })
    #     pkgs.libvdpau-va-gl
    #   ];
    # # force intel-media-driver
    # environment.sessionVariables = { LIBVA_DRIVER_NAME = "iHD"; };

    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "steam"
      "steam-original"
      "steam-runtime"
      "steam-run"
      "steam-unwrapped"
    ];

    hardware.bluetooth.enable = true;
    services.blueman.enable = true;

    services.safeeyes.enable = true;

    # i18n = {
      # inputMethod = {
      #   enabled = "fcitx5";

        # TODO: broken because python 2.7
        # Japanese input
        # fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
      # };
    # };

    ######
    # Nix

    vuizvui.modifyNixPath = false;
    nix.nixPath = [
      "vuizvui=${myLib.philip.home}/vuizvui"
      "nixpkgs=${myLib.philip.home}/nixpkgs"
      # TODO: nicer?
      "nixos-config=${pkgs.writeText "rolery-configuration.nix" ''
        (import <vuizvui/machines>).profpatsch.rolery.config
      ''}"
    ];

    nix.distributedBuilds = true;
    nix.settings.trusted-users = [ "@wheel" ];
    nix.buildMachines = [
      # possehl analytics remote builder
      # {
      #   hostName = "headcounter";
      #   protocol = "ssh"; # TODO: ssh-ng?
      #   maxJobs = 24;
      #   speedFactor = 1000;
      #   system = "x86_64-linux";
      #   supportedFeatures = [ "kvm" "big-parallel" "nixos-test" "benchmark" ];
      # }
      # access to the nix-community aarch64 build box
      # {
      #   hostName = "aarch64.nixos.community";
      #   maxJobs = 64;
      #   sshKey = "/root/aarch64-build-box/ssh-key";
      #   sshUser = "Profpatsch";
      #   system = "aarch64-linux";
      #   supportedFeatures = [ "big-parallel" ];
      # }
      # possehl analytics remote builder
      # {
      #   hostName = "build01.possehl-analytics.com";
      #   maxJobs = 24;
      #   sshKey = "/root/.ssh/possehl-analytics-nix-builder";
      #   sshUser = "nix-builder";
      #   system = "x86_64-linux";
      #   supportedFeatures = [ "kvm" "big-parallel" "nixos-test" "benchmark" ];
      # }
    ];

    nix.settings.builders-use-substitutes = true;
    nix.settings.auto-optimise-store = true;
    nix.settings.trusted-public-keys = [
      "headcounter.org:/7YANMvnQnyvcVB6rgFTdb8p5LG1OTXaO+21CaOSBzg="
    ];
    # nix.settings.substituters = [
    #   "https://digitallyinduced.cachix.org"
    # ];
    # nix.settings.trusted-public-keys = [
    #   "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
    # ];
    # nix.settings.substituters = [ "ssh://nix-ssh@whitby.tvl.fyi" ];
    # nix.settings.trusted-public-keys = [ "cache.tvl.fyi:fd+9d1ceCPvDX/xVhcfv8nAa6njEhAGAEe+oGJDEeoc=" ];

    nix.extraOptions = ''
      builders-use-substitutes = true
      auto-optimise-store = true
    '';

    ##########
    # Network

    networking.hostName = "rolery";

    networking.networkmanager.enable = true;

    networking.hosts = {
      "127.0.0.1" = [
        "me"
        "hoogle.me"
        "torrent.me"
        # keycloak workaround
        "showroom.myants.online"
     ];
    };

    # services.nginx = {
    #   enable = true;
    #   defaultListenAddresses = [ "127.0.0.1" ];
    #   virtualHosts = {
    #     "hoogle.me" = {
    #       enableACME = false;
    #       locations."/".proxyPass = "http://localhost:9090";
    #     };
    #     "torrent.me" = {
    #       enableACME = false;
    #       locations."/".proxyPass = "http://localhost:9091";
    #     };
    #   };

    # };

    services.tailscale = {
      enable = true;
      interfaceName = tailscaleInterface;
    };

    networking.firewall.interfaces.${tailscaleInterface} = {
      allowedTCPPorts = [
        # Open ports that should be accessible via tailscale VPN here
      ];
    } ;

    services.mullvad-vpn.enable = true;

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
    # Backups

    services.btrbk = {
      instances.btrbk = {
        onCalendar = "daily";
        settings = {

        };
      };

    };

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
        sshfs-fuse            # mount ssh machines
        # TODO move into atool deps
        unzip                # extract zip archives
        networkmanagerapplet # for nm-connection-editor
        pkgs.vuizvui.profpatsch.blight  # increase/decrease screen brightness
      ];
      xPkgs = [
        lock-screen       # lock screen
        libnotify         # notification library
        xclip             # clipboard thingy
        xorg.xkill        # X11 application kill
        xorg.xev
        xbindkeys
      ];
      guiPkgs = [
        adwaita-icon-theme
        # TODO: get themes to work. See notes.org.
        gnome-themes-extra
        gsettings-desktop-schemas
        glib # for gsettings
        # can go away once I migrate to pw-ctl and such
        pavucontrol
        pulseaudio
        pika-backup
        linphone
        calibre
      ];
      programmingTools = [
        github-cli                   # official github cli

        # TODO: move to user config
        direnv
        xh                       # reimplementation of httpie in rust (faster startup)
        jq                       # json filter
        inetutils                # tcp debugging
        # TODO: make static binaries
        pkgs.vuizvui.profpatsch.nix-http-serve # serve nix builds and rebuild on reloads
        pkgs.vuizvui.profpatsch.nman # open man pages in temporary nix shell
        # pkgs.vuizvui.profpatsch.watch-server # restart server on code change
        pkgs.vuizvui.profpatsch.until        # restart until cmd succeeds
        execline
        pkgs.dhall
        llm
      ];
      documentation = [
        # mustache-spec NOT IN 16.09
      ];
      userPrograms = [
        audacity lame.lib    # audio editor and mp3 codec
        firefox              # browser
        chromium             # second browser for good measure
        cups                 # print tools, mainly for lp(1)
        pkgs.vuizvui.profpatsch.droopy # simple HTML upload server
        imv                  # young brother of feh and less meh
        inkscape             # vector graphics
        mediainfo            # Swiss army knife of media metadata file information
        myPkgs.mpv           # you are my sun and my stars, and you play my stuff.
        pass                 # standard unix password manager
        poppler_utils        # pdfto*
        sqlite-interactive sqlite-utils datasette
        zathura              # pdf viewer
        ghc                  # powerful pocket calculator
        libreoffice
        dfu-util
        television           # fuzzy finding as TUI
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
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.read-qr-code;
          name = "read-qr-code";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.read-qr-code-from-camera {
            videoDevice = laptop.cameraDevice;
          };
          name = "read-qr-code-from-camera";
        })
        backlight      # adjust laptop backlight
        sfttime        # geek time
      ];
      mailPkgs = [
        elinks               # command line browser
        claws-mail           # mail client
      ];
      nixPkgs = [
        # nix-diff                  # structurally diff two derivations
        nix-prefetch-scripts      # prefetch store paths from various destinations
        # pkgs.vuizvui.taalo-build  # build derivation on taalo
      ];
      tmpPkgs = [
        # TODO needs user service
        redshift   # increases screen warmth at night (so i don’t have to feel cold)
        # pdfjam is the best CLI pdf modification suite
        (texlive.combine { inherit (texlive) scheme-small pdfjam; })
        # move script/nix-cache-binary to here
        cdb
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.e;
          name = "e";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.nix-run;
          name = "nix-run";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.nix-run-bin;
          name = "nix-run-bin";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.nix-eval;
          name = "nix-eval";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.deploy;
          name = "deploy";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.xrandr.two-monitor-setup;
          # this is still referenced in my .xbindkeysrc, which is not in my nixpkgs config
          name = "monitor-home";
        })
        (pkgs.vuizvui.profpatsch.binify {
          exe = pkgs.vuizvui.profpatsch.xrandr.laptop-monitor-only-setup;
          # this is still referenced in my .xbindkeysrc, which is not in my nixpkgs config
          name = "monitor-laptop-only";
        })
        # (pkgs.vuizvui.profpatsch.binify {
        #   exe = pkgs.vuizvui.profpatsch.lyric.lyric;
        #   name = "lyric";
        # })
        # (pkgs.vuizvui.profpatsch.binify {
        #   exe = pkgs.vuizvui.profpatsch.alacritty.alacritty;
        #   name = "terminal-emulator";
        # })
        # myPkgs.zoomboxed
        # for xte with xbindkeys
        xautomation
      ];
    in systemPkgs ++ xPkgs ++ guiPkgs
    ++ programmingTools ++ documentation
    ++ userPrograms ++ userScripts
    ++ mailPkgs ++ nixPkgs ++ tmpPkgs;

    ###########
    # Services


    # Automount
    services.udisks2.enable = true;

    services.logind.extraConfig = lib.generators.toKeyValue {} {

      # I don’t want the machine to go off immediately
      # when I accidentally touch the power button.
      HandlePowerKey = "ignore";
      HandlePowerKeyLongPress = "poweroff";

      # want to be able to listen to music while laptop closed
      LidSwitchIgnoreInhibited = "no";

    };

    services.gnome.gnome-keyring.enable = true;
    # TMP

    # vuizvui.services.guix.enable = true;


    ###################
    # Graphical System

    services.xserver = {
      videoDrivers = [ "amdgpu" ];
    };

    fonts = {
      packages = [
        unfreeAndNonDistributablePkgs.corefonts
        pkgs.source-han-sans
        pkgs.source-code-pro
        pkgs.hasklig
        pkgs.dejavu_fonts
        pkgs.ubuntu_font_family
        pkgs.league-of-moveable-type
        pkgs.noto-fonts-emoji
        # pkgs.zbalermorna
      ];

      enableDefaultPackages = true;
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

    # services.printing = {
    #   enable = true;
    #   drivers = [
    #     pkgs.gutenprint
    #     pkgs.gutenprintBin
    #     # pkgs.hplip
    #     unfreeAndNonDistributablePkgs.canon-cups-ufr2
    #     unfreeAndNonDistributablePkgs.dcp9020cdwlpr
    #   ];
    # };

    # for discovering ddns printers.
    services.avahi.enable = true;
    # for being able to talk to ddns printers after finding them.
    # disable v6 for mdns (argh)
    services.avahi.nssmdns4 = false;
    system.nssModules = [ pkgs.nssmdns ];
    system.nssDatabases.hosts = (lib.mkMerge [
      (lib.mkBefore [ "mdns4_minimal [NOTFOUND=return]" ]) # before resolve
      (lib.mkAfter [ "mdns4" ]) # after dns
    ]);

    ###########
    # Programs

    # required in addition to lock-screen for PAM module
    programs.i3lock.enable = true;

    programs.adb.enable = true;

    vuizvui.programs.gnupg = {
      enable = true;
      agent = {
        enable = true;
        sshSupport = true;
        # I only want to unlock it once per session
        extraConfig = ''
          default-cache-ttl 34560000
          default-cache-ttl-ssh 34560000
          max-cache-ttl 34560000
          max-cache-ttl-ssh 34560000
        '';
      };
    };

    vuizvui.user.profpatsch.programs.scanning = {
      enable = true;
    };

    # some Gnome-infested programs break without dconf
    programs.dconf.enable = true;

    virtualisation.docker = {
      enable = true;
      # # disable docker circumventing our firewall rules.
      # daemon.settings = {
      #   iptables = false;
      # };
    };
    networking.firewall.extraCommands = ''
      # Add a rule to allow docker br-* interfaces to access the `docker0` interface. Let’s hope these IP addresses are stable … TODO: set the docker interface IP space explicitely from this config. Make sure it doesn’t clash with WIFIONICE.
      iptables \
        --insert INPUT \
        --protocol all \
        --source 172.0.0.0/8 \
        --destination 172.17.0.1 \
        --jump ACCEPT
    '';
    # virtualisation.virtualbox.host.enable = true;
    # virtualisation.libvirtd.enable = true;

    #######
    # Misc

    security.pki.certificateFiles = [ "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];

    ########
    # Fixes

    # fix for emacs ssh
    programs.bash.promptInit = "PS1=\"# \"";

    ################
    # User services
    vuizvui.user.profpatsch.services.dunst = {
      enable = true;
      # verbosity = "debug";
      settings = {
        global = {
          width = "(5000, 10000)";
          format = "%s %b";
          origin = "top-left";
          font = "Sans 18";
          markup = "full";
          padding = 5;
          horizontal_padding = 5;
          # please just at the top of the screen for easy access
          offset = "0x0";
          frame_width = 0;
          # follow keyboard focus
          follow = "keyboard";
          max_icon_size = 45;
        };
      };
      iconTheme = {
        package = pkgs.adwaita-icon-theme;
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
        # services.mbsync = {
        #   description = "mbsync job";
        #   wants = [ "notmuch.service" ];
        #   before = [ "notmuch.service"];
        #   path = [ pkgs.pass ];
        #   serviceConfig = {
        #     Restart = "no";
        #     ExecStart = "${pkgs.isync}/bin/mbsync -a";
        #     };
        # };
        # timers.mbsync = {
        #   description = "run mbsync job every 15 minutes";
        #   wantedBy = [ "timers.target" ];
        #   timerConfig = {
        #     OnStartupSec="30s";
        #     OnUnitActiveSec ="15m";
        #   };
        # };
        # services.mu = {
        #   description = "mu job";
        #   serviceConfig = {
        #     Restart = "no";
        #     ExecStart = "${pkgs.notmuch}/bin/notmuch new";
        #   };
        # };

        services.calendar-sync = {
          description = "syncronize private calendars";
          serviceConfig = {
            Restart = "no";
            ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer -vdebug sync";
          };
        };
        timers.calendar-sync = {
          description = "sync calendars every 15 minutes";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnStartupSec="30s";
            OnUnitActiveSec ="15m";
          };
        };

        # I could fight against udev or I could just run this script every ten seconds
        timers.set-keyboard-speed = {
          description = "set the keyboard speed every 10 seconds in case a keyboard was plugged in";
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnStartupSec = "10s";
            OnUnitActiveSec = "10s";
          };
        };
        services.set-keyboard-speed = {
          description = "set the keyboard speed";
          serviceConfig = {
            Type = "oneshot";
            ExecStart = "${pkgs.xorg.xset}/bin/xset r rate 250 35";
          };
        };

        # services.barrel-roll = {
        #   description = "do a barrel roll";
        #   serviceConfig = {
        #     Restart = "no";
        #     ExecStart = "${pkgs.writers.writePython3 "barrel-roll" {
        #       flakeIgnore = [ "E121" "E128" "E999" "E203" "E201" "E202" "E501" ];
        #     } ''
        #       import random
        #       import subprocess as sub
        #       excercises = [
        #          { "name": "burpies"
        #          , "sets": 2
        #          , "reps": 5
        #          , "sigma": 2 },
        #          { "name": "stretch & bend"
        #          , "sets": 1
        #          , "reps": 10
        #          , "sigma": 2 },
        #       ]
        #       ex = random.choice(excercises)
        #       reps = round(random.gauss(ex['reps'], ex['sigma']))
        #       msg = "Do a barrel roll! {} {}, {} sets".format(reps, ex['name'], ex['sets'])
        #       sub.check_call([
        #         "${pkgs.libnotify}/bin/notify-send",
        #         "--urgency=critical",
        #         msg
        #       ])
        #     ''}";
        #     };
        # };
        # timers.barrel-roll = {
        #   description = "do a barrel roll every hour";
        #   wantedBy = [ "timers.target" ];
        #   timerConfig = {
        #     OnStartupSec="10m";
        #     OnUnitActiveSec ="3h";
        #     AccuracySec="15m";
        #   };
        # };

        # services.tagtime = {
        #   description = "tagtime daemon";
        #   wantedBy = [ "default.target" ];
        #   serviceConfig = {
        #     Restart = "on-failure";
        #     ExecStart = "${pkgs.tagtime}/bin/tagtimed";
        #     Environment = [
        #       "DISPLAY=:0"
        #       ''
        #         EDITOR=${pkgs.writers.writeDash "emacs-frame" ''
        #           ${pkgs.emacs}/bin/emacsclient -c "$@"
        #         ''}
        #       ''
        #     ];
        #   };
        # };
      }

    ];

  };
}
