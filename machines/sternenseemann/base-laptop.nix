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

    services.earlyoom.enable = true;

    programs.mosh.enable = true;

    environment = let
      tvl = pkgs.vuizvui.tvl.users.sterni;
    in {
      systemPackages = with pkgs; [
        lowdown
        lynx
        zip unzip
        ripgrep
        nmap
        ffmpeg graphicsmagick
        pavucontrol
        direnv
        tvl.git-only-push
        tvl.acme.plan9port.g
      ] ++ pkgs.vuizvui.sternenseemann.scripts.default;

      variables = {
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

    networking = {
      firewall.enable = true;
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
