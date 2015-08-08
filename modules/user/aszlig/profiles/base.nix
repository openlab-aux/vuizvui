{ config, pkgs, lib, ... }:

let
  cfg = config.vuizvui.user.aszlig.profiles.base;

in {
  options.vuizvui.user.aszlig.profiles.base = {
    enable = lib.mkEnableOption "Base profile for aszlig";
  };

  config = lib.mkIf cfg.enable {
    nix = {
      package = pkgs.nixUnstable;
      useChroot = true;
      readOnlyStore = true;
      extraOptions = ''
        build-cores = 0
        auto-optimise-store = true
      '';
    };

    boot.loader.grub = {
      enable = true;
      version = 2;
    };

    hardware.cpu.intel.updateMicrocode = true;

    users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    networking.wireless.enable = false;
    networking.firewall.enable = false;
    networking.useNetworkd = true;

    i18n.consoleKeyMap = "dvorak";
    i18n.consoleFont = "lat9w-16";

    programs.ssh.startAgent = false;
    programs.ssh.extraConfig = ''
      ServerAliveInterval 60
    '';

    vuizvui.user.aszlig.programs.vim.enable = true;
    vuizvui.user.aszlig.programs.zsh.enable = true;

    services.nixosManual.showManual = false;

    environment.systemPackages = with pkgs; [
      binutils
      cacert
      file
      htop
      iotop
      psmisc
      unrar
      unzip
      vlock
      wget
      xz
    ];

    nixpkgs.config = {
      pulseaudio = true;
      chromium.enablePepperFlash = true;
      firefox.jre = true;

      # Needed for CPU microcode
      allowUnfree = true;

      allowBroken = true;

      packageOverrides = pkgs: {
        miro = pkgs.miro.override {
          enableBonjour = true;
        };
        netrw = pkgs.netrw.override {
          checksumType = "mhash";
        };
        uqm = pkgs.uqm.override {
          use3DOVideos = true;
          useRemixPacks = true;
        };
        w3m = pkgs.w3m.override {
          graphicsSupport = true;
        };
      };

      haskellPackageOverrides = lib.const (super: {
        hinotify = super.hinotify.overrideDerivation (lib.const {
          doCheck = false;
        });
      });
    };

    system.fsPackages = with pkgs; [ sshfsFuse ];
    time.timeZone = "Europe/Berlin";
  };
}
