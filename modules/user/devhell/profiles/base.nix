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
      pulseaudio = {
        enable = true;
        systemWide = false;
      };
    };

    networking = {
      firewall.enable = false;
      useDHCP = false;
    };

    users.users.dev = {
      isNormalUser = true;
      extraGroups = [ "nitrokey" "plugdev" "docker" "vboxusers" "wheel" "mpd" "libvirtd" "wireshark" "video" "audio" ];
      uid = 1000;
      shell = "${pkgs.zsh}/bin/zsh";
    };

    programs = {
      gnupg = {
        agent.enable = true;
        agent.pinentryFlavor = "gnome3";
      };
      ssh = {
        startAgent = false;
      };
      zsh = {
        enable = true;
        enableCompletion = true;
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
      shells = [ "/run/current-system/sw/bin/zsh" ];
    };

    fonts = {
      fontconfig = {
        enable = true;
        useEmbeddedBitmaps = true;
      };
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        cascadia-code
        clearlyU
        cm_unicode
        corefonts
        cozette
        dejavu_fonts
        dosemu_fonts
        fira-code
        font-awesome
        freefont_ttf
        google-fonts
        hack-font
        inconsolata
        junicode
        powerline-fonts
        proggyfonts
        siji
        source-code-pro
        source-sans-pro
        source-serif-pro
        terminus_font
        tewi-font
        tt2020
        ttf_bitstream_vera
        ubuntu_font_family
        ultimate-oldschool-pc-font-pack
        unifont
        vistafonts
        wqy_microhei
      ] ++ lib.filter lib.isDerivation (lib.attrValues lohit-fonts);
    };
  };
}
