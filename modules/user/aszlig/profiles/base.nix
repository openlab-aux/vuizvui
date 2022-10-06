{ config, pkgs, unfreePkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.profiles.base;

in {
  options.vuizvui.user.aszlig.profiles.base = {
    enable = lib.mkEnableOption "Base profile for aszlig";
  };

  config = lib.mkIf cfg.enable {
    nix.readOnlyStore = true;
    nix.settings = {
      sandbox = true;
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
    };

    users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    networking.wireless.enable = false;
    networking.firewall.enable = false;
    networking.useNetworkd = true;
    networking.useDHCP = false;

    services.xserver.layout = "dvorak";
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

    services.openssh.passwordAuthentication = lib.mkOverride 500 false;
    services.openssh.permitRootLogin = lib.mkOverride 500 "no";
    services.openssh.kbdInteractiveAuthentication = lib.mkOverride 500 false;

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

    nixpkgs.overlays = lib.singleton (self: super: {
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

      # Fix for segfault during config validation:
      # https://github.com/i3/i3/pull/5173
      i3 = super.i3.overrideAttrs (drv: {
        patches = lib.singleton (self.fetchpatch {
          url = "https://github.com/i3/i3/commit/"
              + "d0ef4000e9f49d2ef33d6014a4b61339bb787363.patch";
          hash = "sha256-Njdl/XnBypxg2Ytk/phxVfYhdnJHgDshLo9pTYk5o2I";
        });
      });
    });

    system.fsPackages = with pkgs; [ sshfs-fuse ];
    time.timeZone = "Europe/Berlin";
  };
}
