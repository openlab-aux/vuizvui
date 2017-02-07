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
        log-servers = https://headcounter.org/hydra/log
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
    vuizvui.enableGlobalNixpkgsConfig = true;

    services.nixosManual.showManual = false;

    services.journald.extraConfig = ''
      MaxRetentionSec=3month
    '';

    environment.systemPackages = with pkgs; [
      binutils
      cacert
      file
      htop
      iotop
      psmisc
      unfreePkgs.unrar
      unzip
      vlock
      wget
      xz
    ];

    nixpkgs.config = {
      pulseaudio = true;
      firefox.icedtea = true;

      allowBroken = true;

      packageOverrides = pkgs: {
        beets = pkgs.beets.override {
          enableAlternatives = true;
        };
        miro = pkgs.miro.override {
          enableBonjour = true;
        };
        netrw = pkgs.netrw.override {
          checksumType = "mhash";
        };
        nix = pkgs.nixUnstable;
        # XXX: As of edolstra/nix-repl@8a2f5f0, this won't build with
        #      nixUnstable (version 1.12pre4509_69f28eb).
        nix-repl = pkgs.nix-repl.overrideDerivation (drv: {
          src = pkgs.fetchFromGitHub {
            owner = "edolstra";
            repo = "nix-repl";
            rev = "0e49f941205769852846cb8afa228831cf6ae363";
            sha256 = "0zsxkxypnm8zzzslvcdqips7glbxw1riq9mxn9w23kayl8d1mfpx";
          };
          postPatch = (drv.postPatch or "") + ''
            sed -i -e 's/pid\.wait(true)/pid.wait()/g' nix-repl.cc
          '';
        });
        uqm = pkgs.uqm.override {
          use3DOVideos = true;
          useRemixPacks = true;
        };
        w3m = pkgs.w3m.override {
          graphicsSupport = true;
        };
      };
    };

    system.fsPackages = with pkgs; [ sshfsFuse ];
    time.timeZone = "Europe/Berlin";
  };
}
