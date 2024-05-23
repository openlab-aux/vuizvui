{ config, pkgs, unfreePkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.profiles.base;

in {
  options.vuizvui.user.aszlig.profiles.base = {
    enable = lib.mkEnableOption "Base profile for aszlig";
  };

  config = lib.mkIf cfg.enable {
    boot.readOnlyNixStore = true;

    nix.settings = {
      sandbox = true;
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };

    i18n.defaultLocale = "en_US.UTF-8";
    i18n.extraLocaleSettings = {
      LC_ADDRESS = "de_DE.UTF-8";
      LC_MEASUREMENT = "de_DE.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_NAME = "de_DE.UTF-8";
      LC_NUMERIC = "C.UTF-8";
      LC_PAPER = "de_DE.UTF-8";
      LC_TELEPHONE = "de_DE.UTF-8";
      LC_TIME = "C.UTF-8";
    };

    users.defaultUserShell = "/run/current-system/sw/bin/zsh";

    networking.wireless.enable = false;
    networking.firewall.enable = false;
    networking.useNetworkd = true;
    networking.useDHCP = false;

    services.xserver.layout = "us";
    services.xserver.xkbVariant = "dvorak";
    console.useXkbConfig = true;
    console.font = "lat9w-16";

    programs.ssh.startAgent = false;
    programs.ssh.extraConfig = ''
      ServerAliveInterval 60
    '';

    vuizvui.user.aszlig.programs.zsh.enable = true;
    vuizvui.enableGlobalNixpkgsConfig = true;

    services.journald.extraConfig = ''
      MaxRetentionSec=3month
    '';

    services.openssh.settings = {
      PasswordAuthentication = lib.mkOverride 500 false;
      PermitRootLogin = lib.mkOverride 500 "no";
      KbdInteractiveAuthentication = lib.mkOverride 500 false;
    };

    environment.systemPackages = with pkgs; [
      binutils
      cacert
      ddrescue
      file
      htop
      iotop
      moreutils
      vuizvui.aszlig.nlast
      psmisc
      unfreePkgs.unrar
      unzip
      vlock
      vuizvui.aszlig.vim
      wget
      xz
    ];

    nixpkgs.config = {
      pulseaudio = true;
      allowBroken = true;
    };

    nixpkgs.overlays = lib.singleton (lib.const (super: {
      netrw = super.netrw.override {
        checksumType = "mhash";
      };
      nix = super.nixVersions.latest;
      uqm = super.uqm.override {
        use3DOVideos = true;
        useRemixPacks = true;
      };
      w3m = super.w3m.override {
        graphicsSupport = true;
      };
    }));

    system.fsPackages = with pkgs; [ sshfs-fuse ];
    time.timeZone = "Europe/Berlin";
  };
}
