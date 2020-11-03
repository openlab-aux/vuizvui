{ config, pkgs, lib, ... }:

let
	cfg = config.vuizvui.user.devhell.programs.vim;

  environment.variables = { EDITOR = "vim"; };

  environment.systemPackages = with pkgs; [
    (vim_configurable.override.customize {
      name = "vim";
      vimrcConfig.packages.myplugins = with pkgs.vimPlugins; {
        start = [
					awesome-color-scheme
          calendar
          delimitMate
          goyo
          i3config-vim
          latex-box
          lightline
          limelight
          vim-addon-nix
          vim-nix
          vim-signify
          vim-sensible
        ];
        opt = [];
      };
      vimrcConfig.customRC = ''
        set nocompatible
        set showcmd
        set showmatch
        set ignorecase
        set smartcase
        set incsearch
        set smarttab
        set expandtab
        set smartindent
        set ruler
        set tabstop = 4
        set softtabstop = 4
        set textwidth = 79
        set background = dark
        set history = 500
        set backspace = indent,eol,start
      '';
    }
  )];

in {
	options.vuizvui.user.devhell.programs.vim = {
		enable = lib.mkEnableOption "devhell's Vim";
	};

	config = lib.mkIf cfg.enable {
		environment.systemPackages = [ pkgs.vim ];
	};
}
