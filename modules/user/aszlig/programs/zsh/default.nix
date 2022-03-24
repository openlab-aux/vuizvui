{ config, lib, ... }:

with lib;

let
  cfg = config.vuizvui.user.aszlig.programs.zsh;
  inherit (cfg) machineColor;

in {
  options.vuizvui.user.aszlig.programs.zsh = {
    enable = lib.mkEnableOption "zsh";

    machineColor = lib.mkOption {
      type = lib.types.enum [
        "black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"
      ];
      default = "red";
      example = "green";
      description = ''
        The color used for coloring the machine name in the prompt.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.shellInit = ''
      export EDITOR="vim"
      export EMAIL="aszlig@nix.build"
    '';

    nixpkgs.overlays = lib.singleton (lib.const (super: {
      zsh = super.zsh.overrideAttrs (drv: {
        postConfigure = (drv.postConfigure or "") + ''
          sed -i -e '/^name=zsh\/newuser/d' config.modules
        '';
      });
    }));

    programs.zsh.enable = true;

    programs.zsh.shellAliases.t = "task";
    programs.zsh.shellAliases.p = "gopass";

    programs.zsh.setOptions = lib.mkForce [
      "auto_cd"
      "auto_pushd"
      "beep"
      "correct"
      "dvorak"
      "extended_glob"
      "extended_history"
      "hist_fcntl_lock"
      "hist_ignore_dups"
      "hist_no_store"
      "hist_reduce_blanks"
      "interactive_comments"
    ];

    programs.zsh.interactiveShellInit = let
      # This is to make sure that all themes use either color names or RGB
      # colors, otherwise they're not displayed correctly on 24bit color terms.
      highlighter = pkgs.zsh-fast-syntax-highlighting.overrideAttrs (drv: {
        fixThemeColors = pkgs.writers.writePython3 "fix-theme-colors" {
          libraries = [ pkgs.python3Packages.plumbum ];
          flakeIgnore = [ "E111" "E121" "E302" "E305" ];
        } ''
          import re
          from glob import glob
          from plumbum.colorlib.styles import Color
          from configparser import RawConfigParser

          def fix_color(color: str) -> str:
            if color[:2] in ('fg', 'bg') and color[2:3] in ':=':
              return color[:3] + fix_color(color[3:])
            return Color(int(color)).hex_code if color.isdigit() else color

          mainfile = re.sub(
            r'^(:\s+\''${FAST_HIGHLIGHT_STYLES\[[^]]+\]:=)([^}]+)',
            lambda m: m.group(1) + fix_color(m.group(2)),
            open('fast-highlight').read(),
            flags=re.MULTILINE
          )
          open('fast-highlight', 'w').write(mainfile)

          for themefile in glob('themes/*.ini'):
            parser = RawConfigParser(strict=False)
            parser.read(themefile)

            for name, section in parser.items():
              for key, color in section.items():
                section[key] = ','.join(fix_color(c) for c in color.split(','))

            with open(themefile, 'w') as fp:
              parser.write(fp)
        '';
        postPatch = (drv.postPatch or "") + ''
          "$fixThemeColors"
        '';
      });
    in lib.mkAfter ''
      export HISTFILE=~/.histfile
      export HISTSIZE=100000
      export SAVEHIST=100000
      export KEYTIMEOUT=1

      bindkey -v
      if [[ "$TERM" = xterm* ]]; then
        bindkey -v '\e[H' vi-beginning-of-line
        bindkey -v '\e[F' vi-end-of-line

        function set-title() {
          echo -en "\e]2;$2\a"
        }

        function reset-title() {
          echo -en "\e]2;''${(%):-%~}\a\a"
        }

        autoload -Uz add-zsh-hook
        add-zsh-hook preexec set-title
        add-zsh-hook precmd reset-title
      else
        bindkey -v '\e[1~' vi-beginning-of-line
        bindkey -v '\e[4~' vi-end-of-line
      fi

      bindkey -a '/' history-incremental-pattern-search-backward
      bindkey -a '?' history-incremental-pattern-search-forward
      bindkey '\e[A' up-line-or-history
      bindkey '\e[B' down-line-or-history

      zstyle ':completion:*' completer _expand _complete _ignored _approximate
      zstyle ':completion:*' expand prefix suffix
      zstyle ':completion:*' group-name '''
      zstyle ':completion:*' insert-unambiguous true
      zstyle ':completion:*' list-colors '''
      zstyle ':completion:*' list-prompt \
        %SAt %p: Hit TAB for more, or the character to insert%s
      zstyle ':completion:*' list-suffixes true
      zstyle ':completion:*' matcher-list ''' \
        'm:{[:lower:]}={[:upper:]}' \
        'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' \
        'l:|=* r:|=*' \
        'r:|[._-]=** r:|=**'
      zstyle ':completion:*' max-errors 2 numeric
      zstyle ':completion:*' menu select=long
      zstyle ':completion:*' original true
      zstyle ':completion:*' preserve-prefix '//[^/]##/'
      zstyle ':completion:*' prompt \
        'Hm, did you mistype something? There are %e errors in the completion.'
      zstyle ':completion:*' select-prompt \
        %SScrolling active: current selection at %p%s
      zstyle ':completion:*' use-compctl false
      zstyle ':completion:*' verbose true

      autoload -Uz zmv

      source ${lib.escapeShellArg "${highlighter}/${
        "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh"
      }"}
    '';

    programs.zsh.promptInit = ''
      autoload -Uz prompt_special_chars

      () {
          local p_machine='%(!..%B%F{red}%n%b%F{blue}@)%b%F{${machineColor}}%m'
          local p_path='%B%F{blue}[%F{cyan}%~%B%F{blue}]'
          local p_exitcode='%F{green}%?%(!.%F{cyan}>.%b%F{green}>)%b%f '
          PROMPT="$p_machine$p_path$p_exitcode"
      }
    '';
  };
}
