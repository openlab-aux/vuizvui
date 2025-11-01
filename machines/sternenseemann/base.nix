{ config, lib, pkgs, ... }:

let

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  bins = getBins pkgs.less [ "less" ];

in {
  config = {
    boot.tmp.cleanOnBoot = true;

    nix = {
      # waiting on https://github.com/tvlfyi/nix/pull/5
      package = pkgs.lix.override { withAWS = false; };
      # package = pkgs.nix_2_3.override { withAWS = false; };

      settings = {
        sandbox = true;
        gc-keep-derivations = false;
        builders-use-substitutes = true;
        # allow only trusted users
        allowed-users = [ ];
      };
    };

    nixpkgs = {
      config.allowUnfree = true;
      # Use local depot if rebuilding on a machine where it's available
      overlays = lib.optionals (builtins.pathExists "/home/lukas/src/depot") [
        (self: super: {
          vuizvui = super.vuizvui // {
            tvl = super.vuizvui.tvl.override {
              tvlSrc = /home/lukas/src/depot;
            };
          };
        })
      ];
    };

    services.journald.extraConfig = lib.mkDefault "SystemMaxUse=500M";

    console.keyMap = lib.mkDefault "de-latin1";

    time.timeZone = lib.mkDefault "Europe/Berlin";

    i18n = {
      defaultLocale = "en_US.UTF-8";
      supportedLocales = [
        (config.i18n.defaultLocale + "/UTF-8")
        "C.UTF-8/UTF-8"
        "de_DE.UTF-8/UTF-8"
      ];
    };

    programs.fish = {
      enable = true;
      vendor.completions.enable = true;
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

    environment.systemPackages =
      let
         tvl = pkgs.vuizvui.tvl.users.sterni;
      in
      with pkgs;
      [
        curl wget
        man-pages
        man-pages-posix
        tvl.dot-time-man-pages
        gitFull
        file htop psmisc
        ripgrep
        tvl.acme.plan9port.g
        tvl.git-only-push
      ];

    vuizvui.user.sternenseemann.profiles.editors = {
      enable = true;
      editor = lib.mkDefault "vim";
      vim.enable = lib.mkDefault true;
    };

    environment.variables = {
      PAGER = "${bins.less} -R";
      # git-diff without the extra options passed to less
      GIT_PAGER = bins.less;
      LESS = "-R";
      RIPGREP_CONFIG_PATH = pkgs.writeText "ripgreprc" ''
        --max-columns=150
        --max-columns-preview
        --smart-case
        --hidden
        --glob=!.git/*
      '';
    };

    environment.etc."gitconfig".text = ''
      [user]
          email = sternenseemann@systemli.org
          name = sternenseemann
      [push]
          default = matching
      [pull]
          rebase = true
      [init]
          defaultBranch = canon
      [sendemail]
          smtpEncryption = tls
          smtpServer = mail.systemli.org
          smtpUser = sternenseemann@systemli.org
          smtpServerPort = 587
      [merge]
          conflictstyle = diff3
    '';

  environment.etc."inputrc".text = lib.mkAfter ''
    set search-ignore-case on

    set completion-ignore-case on
    set menu-complete-display-prefix on
    set show-all-if-unmodified on
    set skip-completed-text on

    # https://lists.gnu.org/archive/html/bug-bash/2005-08/msg00003.html
    set bind-tty-special-chars off
    C-w: unix-filename-rubout
  '';
  };
}
