{ pkgs, unfreePkgs, unfreeAndNonDistributablePkgs, config, lib, ... }:

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

    boot.cleanTmpDir = true;
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    environment.systemPackages = [
      pkgs.file
      pkgs.gajim
      pkgs.gimp
      pkgs.git
      pkgs.htop
      pkgs.inkscape
      (unfreeAndNonDistributablePkgs.plasma5Packages.ark.override {
        unfreeEnableUnrar = true;
        inherit (unfreePkgs) unrar;
      })
      pkgs.plasma5Packages.gwenview
      pkgs.plasma5Packages.kaddressbook
      pkgs.plasma5Packages.kate
      pkgs.plasma5Packages.kdepim-addons
      pkgs.plasma5Packages.kleopatra
      pkgs.plasma5Packages.kmail
      pkgs.plasma5Packages.kontact
      pkgs.plasma5Packages.korganizer
      pkgs.plasma5Packages.okular
      pkgs.libreoffice
      pkgs.mpv
      pkgs.skanlite
      pkgs.thunderbird
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

    # For MTP and other stuff.
    services.gvfs.enable = true;

    # Plasma desktop with German keyboard layout.
    services.xserver.enable = true;
    services.xserver.layout = "de";
    services.xserver.xkbOptions = lib.mkOverride 900 "eurosign:e";
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.displayManager.defaultSession = "plasma5";
    services.xserver.desktopManager.plasma5.enable = true;

    # And also most common scanners are also HP ones.
    hardware.sane.enable = true;
    hardware.sane.extraBackends = [
      unfreeAndNonDistributablePkgs.hplipWithPlugin
    ];

    hardware.opengl.driSupport32Bit = true;
    hardware.pulseaudio.enable = true;
    hardware.pulseaudio.package = pkgs.pulseaudioFull;
    sound.enable = true;

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
    vuizvui.system.kernel.zswap.enable = true;
  };
}
