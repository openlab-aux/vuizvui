{ pkgs, unfreeAndNonDistributablePkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption;
  cfg = config.vuizvui.user.aszlig.profiles.managed;
  inherit (cfg) mainUser;

in {
  options.vuizvui.user.aszlig.profiles.managed = {
    enable = mkEnableOption "common profile for aszlig's managed machines";

    mainUser = mkOption {
      example = "foobar";
      description = ''
        Main user account of the managed system.
      '';
    };
  };

  config = mkIf cfg.enable {
    vuizvui.system.kernel.bfq.enable = true;

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    environment.systemPackages = [
      pkgs.bomi
      pkgs.chromium
      pkgs.file
      pkgs.gimp
      pkgs.git
      pkgs.htop
      pkgs.inkscape
      pkgs.kdeApplications.gwenview
      pkgs.kdeApplications.kaddressbook
      pkgs.kdeApplications.kate
      pkgs.kdeApplications.kleopatra
      pkgs.kdeApplications.kmail
      pkgs.kdeApplications.kmix
      pkgs.kdeApplications.korganizer
      pkgs.kdeApplications.okular
      pkgs.libreoffice
      pkgs.mpv
      pkgs.skanlite
      pkgs.thunderbird
      pkgs.vuizvui.aszlig.gajim
      pkgs.vuizvui.aszlig.vim
      pkgs.wine
      pkgs.youtubeDL
      unfreeAndNonDistributablePkgs.skype
    ];

    i18n.consoleUseXkbConfig = true;

    # Printing for the most common printers among the managed machines.
    services.printing.enable = true;
    services.printing.drivers = [
      pkgs.gutenprint
      unfreeAndNonDistributablePkgs.hplipWithPlugin
    ];

    # Plasma desktop with German keyboard layout
    services.xserver.enable = true;
    services.xserver.layout = "de";
    services.xserver.xkbOptions = lib.mkOverride 900 "eurosign:e";
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.desktopManager.plasma5.enable = true;
    services.xserver.desktopManager.xterm.enable = false;

    # And also most common scanners are also HP ones.
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [
      unfreeAndNonDistributablePkgs.hplipWithPlugin
    ];

    hardware.cpu.intel.updateMicrocode = true;
    hardware.opengl.s3tcSupport = true;
    hardware.opengl.driSupport32Bit = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.package = pkgs.pulseaudioFull;

    networking.firewall.enable = false;
    networking.networkmanager.enable = true;

    nix.autoOptimiseStore = true;
    nix.buildCores = 0;
    nix.readOnlyStore = true;
    nix.useSandbox = true;

    nixpkgs.config.chromium.enablePepperFlash = true;
    nixpkgs.config.pulseaudio = true;

    programs.bash.enableCompletion = true;

    services.tlp.enable = true;

    time.timeZone = "Europe/Berlin";

    users.users.${mainUser} = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "networkmanager" "scanner" "video" "wheel" ];
    };

    vuizvui.enableGlobalNixpkgsConfig = true;
  };
}
