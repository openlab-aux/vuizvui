{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.devhell.profiles.base;

in {
  options.vuizvui.user.devhell.profiles.base = {
    enable = lib.mkEnableOption "Base profile for devhell";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      kernelPackages = pkgs.linuxPackages_latest;
      tmp.cleanOnBoot = true;
    };

    nix.settings = {
      sandbox = true;
      cores = 0;
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };

    system = {
      fsPackages = with pkgs; [
        sshfs-fuse
        fuse
        cryptsetup
      ];
    };

    hardware = {
      enableAllFirmware = true;
      nitrokey.enable = true;
      opengl = {
        driSupport32Bit = true;
      };
    };

    networking = {
      firewall.enable = false;
      useDHCP = false;
    };

    security.rtkit.enable = true;

    zramSwap.enable = true;

    users.users.dev = {
      isNormalUser = true;
      extraGroups = [ "nitrokey" "plugdev" "docker" "vboxusers" "wheel" "libvirtd" "wireshark" "video" "audio" ];
      uid = 1000;
      shell = "${pkgs.zsh}/bin/zsh";
    };

    nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
      "steam"
      "steam-original"
      "steam-runtime"
      "steam-run"
    ];

    programs = {
      dconf.enable = true;
      iftop.enable = true;
      iotop.enable = true;
      usbtop.enable = true;
      less.enable = true;
      steam.enable = true;
      tmux.enable = true;
      traceroute.enable = true;
      wireshark.enable = true;
      wavemon.enable = true;
      bandwhich.enable = true;
      git.enable = true;
      liboping.enable = true;
      htop.enable = true;
      gnupg = {
        agent.enable = true;
        agent.enableSSHSupport = true;
        agent.pinentryPackage = "gnome3";
      };
      ssh = {
        startAgent = false;
      };
      bash = {
        enableCompletion = true;
      };
      starship = {
        enable = true;
        settings = {
          command_timeout = 1000;
          nix_shell.disabled = false;
        };
      };
      zsh = {
        enable = true;
        promptInit = ''
          eval "$(${pkgs.starship}/bin/starship init zsh)"
          ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin
        '';
        interactiveShellInit = ''
          zstyle ':completion:*' menu select
          source ${pkgs.fzf}/share/fzf/key-bindings.zsh
        '';
        shellAliases = {
          ls = "lsd";
          grep = "rg";
          ip = "ip --color=auto";
          tree = "broot";
          ps = "procs";
          ack = "ag";
          less = "peep";
        };
        setOptions = [
          "auto_cd"
          "auto_pushd"
          "correct"
          "hist_fcntl_lock"
          "hist_ignore_dups"
          "hist_no_store"
          "hist_reduce_blanks"
        ];
      };
    };

    environment = {
      sessionVariables = {
        XDG_SESSION_TYPE = "wayland";
        XDG_CURRENT_DESKTOP = "sway";
        MOZ_ENABLE_WAYLAND = "1";
        MOZ_USE_XINPUT2 = "1";
        SDL_VIDEODRIVER = "wayland";
        QT_QPA_PLATFORM = "wayland-egl";
        QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
        XKB_DEFAULT_LAYOUT = "gb";
      };
      shellInit = ''
        export GPG_AGENT_INFO=$HOME/.gnupg/S.gpg-agent
        export LIBVIRT_DEFAULT_URI=qemu:///system
        export LS_COLORS="$(vivid generate snazzy)"
        export LESS_TERMCAP_mb=$'\E[1;31m'
        export LESS_TERMCAP_md=$'\E[1;36m'
        export LESS_TERMCAP_me=$'\E[0m'
        export LESS_TERMCAP_so=$'\E[01;33m'
        export LESS_TERMCAP_se=$'\E[0m'
        export LESS_TERMCAP_us=$'\E[1;32m'
        export LESS_TERMCAP_ue=$'\E[0m'
        export EDITOR='vim'
      '';
      shells = [ pkgs.zsh ];
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
    };

    fonts = {
      fontconfig = {
        enable = true;
        useEmbeddedBitmaps = true;
      };
      enableGhostscriptFonts = true;
      packages = let
        mkNerdFont = src: pkgs.stdenv.mkDerivation {
          name = lib.removeSuffix ".zip" src.name;
          inherit src;
          sourceRoot = ".";
          nativeBuildInputs = [ pkgs.unzip ];
          dontBuild = true;
          patchPhase = "find -iname '*Windows Compatible*' -delete";
          installPhase = ''
            find -iname '*.otf' -exec install -vD -m0644 {} \
              "$out/share/fonts/opentype/NerdFonts/{}" \;
            find -iname '*.ttf' -exec install -vD -m0644 {} \
              "$out/share/fonts/truetype/NerdFonts/{}" \;
          '';
        };
      in with pkgs; [
        clearlyU
        fixedsys-excelsior
        cm_unicode
        corefonts
        cozette
        dosemu_fonts
        freefont_ttf
        google-fonts
        junicode
        siji
        tewi-font
        tt2020
        ultimate-oldschool-pc-font-pack
        unifont
        vistafonts
        wqy_microhei
      ] ++ lib.filter lib.isDerivation (lib.attrValues lohit-fonts)
        ++ map mkNerdFont pkgs.nerdfonts.srcs;
    };
  };
}
