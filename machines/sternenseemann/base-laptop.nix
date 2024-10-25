# TODO(sterni): split out common stuff for personal
#               computers when I have a non-laptop one
{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./fonts.nix
  ];

  config = {
    console.keyMap = "neo";

    hardware.bluetooth.enable = lib.mkDefault true;
    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = true;
      socketActivation = true;
    };

    hardware.cpu.intel.updateMicrocode = true;

    powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
    powerManagement.powertop.enable = lib.mkDefault true;

    vuizvui.hardware.thinkpad.enable = lib.mkDefault true;
    services.thermald.enable = true;
    services.upower = {
      enable = true;
      usePercentageForPolicy = true;
      criticalPowerAction = "Hibernate";

      percentageLow = 10;
      percentageCritical = 5;
      percentageAction = 2;
    };

    programs.mosh.enable = true;

    environment = let inherit (pkgs.vuizvui.tvl.users.sterni) emacs; in {
      systemPackages = with pkgs; [
        lr
        lowdown
        lynx
        zip unzip
        ripgrep
        nmap
        ffmpeg graphicsmagick
        pavucontrol
        emacs
        direnv
      ] ++ pkgs.vuizvui.sternenseemann.scripts.default;

      variables = {
        EDITOR = "${emacs}/bin/emacsclient";
        VISUAL = "${emacs}/bin/emacsclient";
        RIPGREP_CONFIG_PATH = pkgs.writeText "ripgreprc" ''
          --max-columns=150
          --max-columns-preview
          --smart-case
          --hidden
          --glob=!.git/*
        '';
      };
    };

    programs.bash.interactiveShellInit = ''
      eval "$(direnv hook bash)"
    '';

    programs.fish.shellInit = ''
      alias e "emacsclient -n"
    '';

    networking = {
      firewall = {
        enable = true;
        allowedTCPPortRanges = [
          { from = 9990; to = 9999; }
        ];
      };
      networkmanager.enable = true;
    };

    services.printing = {
      enable = true;
      drivers = [
        pkgs.gutenprint
        pkgs.hplip
        pkgs.cups-brother-hll2350dw
      ];
    };
  };
}
