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
      cleanTmpDir = true;
    };

    nix = {
      buildCores = 0;
      useSandbox = true;
      extraOptions = ''
        auto-optimise-store = true
        experimental-features = nix-command flakes
      '';
    };

    time = {
      timeZone = "Europe/London";
    };

    system = {
      fsPackages = with pkgs; [
        sshfsFuse
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

    users.users.dev = {
      isNormalUser = true;
      extraGroups = [ "nitrokey" "plugdev" "docker" "vboxusers" "wheel" "libvirtd" "wireshark" "video" "audio" ];
      uid = 1000;
      shell = "${pkgs.zsh}/bin/zsh";
    };

    programs = {
      dconf = {
        enable = true;
      };
      gnupg = {
        agent.enable = true;
        agent.pinentryFlavor = "gnome3";
      };
      ssh = {
        startAgent = false;
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
      bash = {
        enableCompletion = true;
      };
      iftop = {
        enable = true;
      };
      iotop = {
        enable = true;
      };
      less = {
        enable = true;
      };
      tmux = {
        enable = true;
      };
      traceroute = {
        enable = true;
      };
      wireshark = {
        enable = true;
      };
    };

    environment = {
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

    fonts = {
      fontconfig = {
        enable = true;
        useEmbeddedBitmaps = true;
      };
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        clearlyU
        fixedsys-excelsior
        cm_unicode
        corefonts
        cozette
        dosemu_fonts
        freefont_ttf
        google-fonts
        junicode
        nerdfonts
        siji
        tewi-font
        tt2020
        ultimate-oldschool-pc-font-pack
        unifont
        vistafonts
        wqy_microhei
      ] ++ lib.filter lib.isDerivation (lib.attrValues lohit-fonts);
    };
  };
}
