{ pkgs, lib, config, ... }:

let
  cfg = config.vuizvui.user.devhell.programs.vim;

  customVim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig.packages.myplugins = with pkgs.vimPlugins; {
      start = [
        ale
        awesome-vim-colorschemes
        delimitMate
        dracula-vim
        goyo-vim
        i3config-vim
        indentLine
        lightline-vim
        limelight-vim
        sleuth
        tabular
        vim-lastplace
        vim-nix
        vim-signify
      ];
      opt = [ ];
    };
    vimrcConfig.customRC = ''
      set nocompatible
      set showcmd
      set showmatch
      set ignorecase
      set smartcase
      set incsearch
      set modeline
      set smarttab
      set expandtab
      set smartindent
      set ruler
      set tabstop=4
      set softtabstop=4
      set shiftwidth=4
      set textwidth=79
      set background=dark
      set history=500
      set backspace=indent,eol,start
      set nu
      set cursorline
      set laststatus=2
      set signcolumn=number
      set timeout timeoutlen=5000 ttimeoutlen=100

      filetype plugin indent on
      colorscheme tender

      " signify Settings
      set updatetime=100

      " indentLine Settings
      let g:indentLine_char_list = ['|', '¦', '┆', '┊']
      let g:indentLine_setColors=1
      let g:indentLine_fileTypeExclude=['help', 'main', 'vimfiler', 'json']
      let g:better_whitespace_filetypes_blacklist = ['diff', 'gitcommit',
      \ 'help', 'markdown', 'leaderGuide']

      " goyo Settings
      autocmd! User GoyoEnter Limelight
      autocmd! User GoyoLeave Limelight!

      " lightline Settings
      let g:lightline = {'colorscheme': 'deus'}
      set noshowmode
    '';
  };

in {
  options.vuizvui.user.devhell.programs.vim = {
    enable = lib.mkEnableOption "devhell's Vim";
  };

  config = lib.mkIf cfg.enable {
    environment.variables.EDITOR = "vim";
    environment.systemPackages = [ customVim ];
  };
}
