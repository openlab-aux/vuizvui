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

    environment = {
      systemPackages = with pkgs; [
        lowdown
        zip unzip
        nmap
        ffmpeg graphicsmagick
        pavucontrol
        direnv
      ] ++ pkgs.vuizvui.sternenseemann.scripts.default;
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
