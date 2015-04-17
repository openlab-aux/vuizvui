{ config, pkgs, lib, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
  };

  nix = {
    buildCores = 0;
    useChroot = true;
    binaryCaches = [ "https://headcounter.org/hydra/" ];
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

  security = {
    setuidPrograms = [ ];
  };

  programs = {
    ssh = {
      startAgent = false;
    };
    zsh = {
      enable = true;
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
    sessionVariables.TERM = "xterm-256color";
  };

  fonts = {
    enableGhostscriptFonts = true;
    fontconfig.enable = true;
    enableCoreFonts = true;
    fonts = with pkgs; [
      ttf_bitstream_vera
      dejavu_fonts
      dosemu_fonts
      proggyfonts
      vistafonts
      terminus_font
      ubuntu_font_family
      unifont
    ];
  };
}
