{ config, lib, ... }:

with lib;

{
  options.vuizvui.zsh = {
    enable = mkEnableOption "zsh";
  };

  config = mkIf config.vuizvui.zsh.enable {
    environment.shellInit = ''
      export EDITOR="vim"
      export EMAIL="aszlig@redmoonstudios.org"
    '';

    programs.zsh.enable = true;

    programs.zsh.shellAliases.t = "task";

    programs.zsh.interactiveShellInit = mkAfter ''
      export HISTFILE=~/.histfile
      export HISTSIZE=100000
      export SAVEHIST=100000

      unsetopt SHARE_HISTORY

      setopt extendedglob
      setopt extendedhistory
      setopt globcomplete
      setopt histnostore
      setopt histreduceblanks
      setopt correct
      setopt dvorak
      setopt interactivecomments
      setopt autopushd
      setopt autocd
      setopt beep

      bindkey -v
      if [[ "$TERM" = xterm ]]; then
        bindkey -v '\e[H' vi-beginning-of-line
        bindkey -v '\e[F' vi-end-of-line

        function set-title() {
          echo -en "\e]2;$2\a"
        }

        function reset-title() {
          echo -en "\e]2;''${(%):-%~}\a"
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

      autoload -Uz compinit
      compinit

      autoload -Uz zmv
    '';

    programs.zsh.promptInit = ''
      autoload -Uz prompt_special_chars

      () {
          local p_machine='%(!..%B%F{red}%n%b%F{blue}@)%b%F{red}%m'
          local p_path='%B%F{blue}[%F{cyan}%~%B%F{blue}]'
          local p_exitcode='%F{green}%?%(!.%F{cyan}>.%b%F{green}>)%b%f '
          PROMPT="$p_machine$p_path$p_exitcode"
      }
    '';
  };
}
