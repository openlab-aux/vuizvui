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
        minimap-vim
        sleuth
        tabular
#        vim-clap
        vim-illuminate
        vim-better-whitespace
        vim-lastplace
        vim-nix
        vim-mucomplete
        vim-signify
        vim-toml
        vim-hexokinase
        vim-shellcheck
        vim-markdown
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
      if exists('+termguicolors')
        let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
        let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
        set termguicolors
      endif

      filetype plugin indent on
      colorscheme tender

      " mucomplete Settings
      set completeopt+=menuone
      set completeopt+=noinsert
      set shortmess+=c
      set belloff+=ctrlg

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

      " hexokinase Settings
      let g:Hexokinase_highlighters = ['backgroundfull']
      let g:Hexokinase_refreshEvents =
        \ ['TextChanged', 'TextChangedI', 'InsertLeave', 'BufRead']

      " Autoload
      autocmd FileType tex :packadd vimtex
    '';
  };

  postInstall = (drv.postInstall or "") + ''
    ln -sf "$vimrc" "$out/share/vim/vimrc"
  '';
})
