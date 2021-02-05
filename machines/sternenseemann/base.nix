{ config, lib, pkgs, ... }:

{
  config = {
    boot.cleanTmpDir = true;

    nix.useSandbox = true;
    nix.extraOptions = "gc-keep-derivations = false";
    nixpkgs.config.allowUnfree = true;

    services.journald.extraConfig = lib.mkDefault "SystemMaxUse=500M";

    console.keyMap = lib.mkDefault "de-latin1";

    time.timeZone = lib.mkDefault "Europe/Berlin";

    i18n = {
      defaultLocale = "en_US.UTF-8";
    };

    programs.fish.enable = true;
    programs.fish.vendor.completions.enable = true;

    documentation = {
      enable = true;
      dev.enable = true;
      man.enable = true;
    };

    environment.systemPackages = with pkgs; [
      curl wget
      mandoc man-pages
      git
      file htop psmisc tmux
    ];
  };
}
