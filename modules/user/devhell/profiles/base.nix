{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.devhell.profiles.base;

in {
  options.vuizvui.user.devhell.profiles.base = {
    enable = lib.mkEnableOption "Base profile for devhell";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      kernelPackages = pkgs.linuxPackages_4_19;
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
      cpu.intel.updateMicrocode = true;
      opengl = {
        s3tcSupport = true;
        driSupport32Bit = true;
      };
      pulseaudio = {
        enable = true;
        systemWide = false;
      };
    };

    networking.firewall.enable = false;

    users.users.dev = {
      isNormalUser = true;
      extraGroups = [ "plugdev" "docker" "vboxusers" "wheel" "mpd" "libvirtd" "wireshark" ];
      uid = 1000;
      shell = "${pkgs.zsh}/bin/zsh";
    };

    programs = {
      ssh = {
        startAgent = false;
      };
      zsh = {
        enable = true;
        enableCompletion = true;
      };
      bash = {
        enableCompletion = true;
        promptInit = ''
          # Provide a nice prompt.
          PROMPT_COLOR="1;31m"
          let $UID && PROMPT_COLOR="1;32m"
          PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
          if test "$TERM" = "xterm"; then
            PS1="\[\033]2;\h:\u:\w\007\]$PS1"
          fi
          eval `dircolors ~/.dir_colors`
        '';
      };
    };

    environment = {
      shells = [ "/run/current-system/sw/bin/zsh" ];
    };

    fonts = {
      fontconfig = {
        enable = true;
        ultimate = {
          enable = true;
        };
      };
      enableGhostscriptFonts = true;
      enableCoreFonts = true;
      fonts = with pkgs; [
        clearlyU
        cm_unicode
        corefonts
        dejavu_fonts
        dosemu_fonts
        font-awesome_5
        freefont_ttf
        google-fonts
        hack-font
        inconsolata
        powerline-fonts
        proggyfonts
        siji
        source-code-pro
        source-sans-pro
        source-serif-pro
        terminus_font
        tewi-font
        ttf_bitstream_vera
        ubuntu_font_family
        unifont
        vistafonts
        wqy_microhei
      ] ++ lib.filter lib.isDerivation (lib.attrValues lohit-fonts);
    };
  };
}
