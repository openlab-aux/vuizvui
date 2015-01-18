{ config, pkgs, lib, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix.buildCores = 0;
  nix.useChroot = true;

  time.timeZone = "Europe/London";

  system.fsPackages = with pkgs; [
    sshfsFuse
    fuse
    cryptsetup
  ];

  hardware = {
    enableAllFirmware = true;
    cpu.intel.updateMicrocode = true;
    opengl.s3tcSupport = true;
    opengl.driSupport32Bit = true;
    pulseaudio.enable = true;
  };

  security.setuidPrograms = [ ];

  programs.ssh.startAgent = false;
  programs.zsh.enable = true;
  programs.bash.enableCompletion = true;
  programs.bash.promptInit = ''
    # Provide a nice prompt.
    PROMPT_COLOR="1;31m"
    let $UID && PROMPT_COLOR="1;32m"
    PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
    if test "$TERM" = "xterm"; then
      PS1="\[\033]2;\h:\u:\w\007\]$PS1"
    fi
    eval `dircolors ~/.dir_colors`
  '';

  environment.shells = [ "/run/current-system/sw/bin/zsh" ];
  environment.sessionVariables.TERM = "xterm-256color";

  fonts.enableGhostscriptFonts = true;
  fonts.fontconfig.enable = true;
  fonts.enableCoreFonts = true;
  fonts.fonts = [
    pkgs.ttf_bitstream_vera
    pkgs.dejavu_fonts
    pkgs.dosemu_fonts
    pkgs.proggyfonts
    pkgs.vistafonts
    pkgs.terminus_font
    pkgs.ubuntu_font_family
    pkgs.unifont
  ];
}
