{ config, lib, pkgs, ... }:

let

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  bins = (getBins pkgs.neovim [ "nvim" ])
      // (getBins pkgs.less [ "less" ])
      ;

in {
  config = {
    boot.cleanTmpDir = true;

    nix = {
      useSandbox = true;
      extraOptions = ''
        gc-keep-derivations = false
        builders-use-substitutes = true
      '';
    };

    nixpkgs.config.allowUnfree = true;

    services.journald.extraConfig = lib.mkDefault "SystemMaxUse=500M";

    console.keyMap = lib.mkDefault "de-latin1";

    time.timeZone = lib.mkDefault "Europe/Berlin";

    i18n = {
      defaultLocale = "en_US.UTF-8";
    };

    programs.fish = {
      enable = true;
      vendor.completions.enable = true;
      shellAliases = {
        "sd" = "systemctl";
      };
      shellInit = ''
        set -x fish_greeting ""

        # an adisbladis original
        function bonk
          for arg in $argv
            set -l store_path (string unescape (nix-instantiate --eval --expr "with (import <nixpkgs> {}); builtins.toString (lib.getBin $arg)"))
            nix-store --realise "$store_path"
            set PATH "$store_path/bin" $PATH
          end
        end
      '';
    };

    documentation = {
      enable = true;
      dev.enable = true;
      man.enable = false;
    };

    vuizvui.user.sternenseemann.documentation.mandoc = {
      enable = true;
      generateCaches = true;
      manPath = [ "share/man" "share/man/de" ];
    };

    environment.systemPackages = with pkgs; [
      curl wget
      man-pages
      git
      file htop psmisc tmux
      neovim
    ];

    environment.variables = {
      EDITOR = bins.nvim;
      VISUAL = bins.nvim;
      PAGER = "${bins.less} -R";
      # git-diff without the extra options passed to less
      GIT_PAGER = bins.less;
      LESS = "-R";
    };
  };
}
