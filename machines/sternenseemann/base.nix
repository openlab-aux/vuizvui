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
      man = {
        enable = true;
        generateCaches = true;
        man-db.enable = false;
        mandoc = {
          enable = true;
          manPath = [ "share/man" "share/man/de" ];
        };
      };
    };

    # HACK: create man0p, man1p, man3p etc. as directories in the environment
    # out path to work around the makewhatis(8) bug that it always assumes
    # symlinks are files. Since man3p etc. comes from a single package,
    # buildEnv just symlinks the entire directory and makewhatis(8) then
    # ignores it.
    environment.extraSetup = lib.mkBefore ''
      for dir in $out/share/man/*; do
        section="$(basename "$dir")"
        sectionDir="$out/share/man/$section"

        if test -L "$sectionDir"; then
          dest="$(realpath "$sectionDir")"

          if test -d "$dest"; then
            echo "Recreating $sectionDir and linking everything from $dest..." 1>&2

            rm "$sectionDir"
            mkdir "$sectionDir"

            for f in "$dest"/*; do
              ln -s "$f" -t "$sectionDir"
            done
          fi
        fi
      done
    '';

    environment.systemPackages = with pkgs; [
      curl wget
      man-pages
      man-pages-posix
      gitFull
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
