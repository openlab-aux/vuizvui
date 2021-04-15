{ pkgs ? import <nixpkgs> {} }:

pkgs.vim_configurable.overrideAttrs (drv: {
  vimrc = pkgs.vimUtils.vimrcFile {
    packages.myplugins = with pkgs.vimPlugins; {
      start = [
        ale
        awesome-vim-colorschemes
        delimitMate
        goyo-vim
        i3config-vim
        indentLine
        lightline-vim
        limelight-vim
        sleuth
        tabular
        vim-illuminate
        vim-better-whitespace
        vim-lastplace
        vim-nix
        vim-signify
      ];
      opt = [ vimtex ];
    };
    customRC = ''
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
      let g:indentLine_setColors = 1
      let g:indentLine_fileTypeExclude = ['help', 'main', 'vimfiler', 'json']
      let g:better_whitespace_filetypes_blacklist = ['diff', 'gitcommit',
      \ 'help', 'markdown', 'leaderGuide']

      " goyo Settings
      autocmd! User GoyoEnter Limelight
      autocmd! User GoyoLeave Limelight!

      " lightline Settings
      let g:lightline = {'colorscheme': 'deus'}
      set noshowmode

      " vimtex Settings
      let g:tex_flavor = 'latex'

      " Autoload
      autocmd FileType tex :packadd vimtex
    '';
  };

  postInstall = (drv.postInstall or "") + ''
    ln -sf "$vimrc" "$out/share/vim/vimrc"
  '';
})
