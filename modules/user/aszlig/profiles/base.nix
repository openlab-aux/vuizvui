{ config, pkgs, unfreePkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.profiles.base;

in {
  options.vuizvui.user.aszlig.profiles.base = {
    enable = lib.mkEnableOption "Base profile for aszlig";
  };

  config = lib.mkIf cfg.enable {
    nix = {
      useSandbox = true;
      readOnlyStore = true;
      buildCores = 0;
      extraOptions = ''
        auto-optimise-store = true
        experimental-features = nix-command flakes
      '';
    };

    users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    networking.wireless.enable = false;
    networking.firewall.enable = false;
    networking.useNetworkd = true;
    networking.useDHCP = false;

    console.keyMap = "dvorak";
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

    services.openssh.passwordAuthentication = lib.mkDefault false;
    services.openssh.permitRootLogin = lib.mkDefault "no";
    services.openssh.challengeResponseAuthentication = lib.mkDefault false;

    environment.systemPackages = with pkgs; [
      binutils
      cacert
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
      nix = super.nixUnstable;
      uqm = super.uqm.override {
        use3DOVideos = true;
        useRemixPacks = true;
      };
      w3m = super.w3m.override {
        graphicsSupport = true;
      };
    }));

    system.fsPackages = with pkgs; [ sshfsFuse ];
    time.timeZone = "Europe/Berlin";
  };
}
