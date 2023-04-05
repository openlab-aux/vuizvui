{ pkgs ? import <nixpkgs> {} }:

pkgs.neovim-unwrapped.overrideAttrs (drv: {
  vimrc = pkgs.vimUtils.vimrcFile {
    packages.myplugins = with pkgs.vimPlugins; {
      start = [
        mini-nvim
        ccc-nvim
        tender-vim
        vim-nix
        vim-sleuth
        swayconfig-vim
        nvim-lastplace
        delimitMate
        gitsigns-nvim
        zen-mode-nvim
        twilight-nvim
        nvim-web-devicons
        indent-blankline-nvim
      ];
      opt = [ ];
    };
    customRC = ''
     filetype plugin indent on
     set termguicolors
     set autoindent
     set background=dark
     set cc=80
     set cursorline
     set expandtab
     set history=500
     set hlsearch
     set ignorecase
     set incsearch
     set modeline
     set nocompatible
     set number
     set ruler
     set laststatus=2
     set signcolumn=number
     set timeout timeoutlen=5000 ttimeoutlen=100
     set shiftwidth=4
     set showcmd
     set showmatch
     set smartcase
     set smartindent
     set smarttab
     set softtabstop=4
     set tabstop=4
     set ttyfast
     set signcolumn=auto
     syntax on
     colorscheme tender

     lua require('mini.indentscope').setup()
     lua require('mini.trailspace').setup()
     lua require('mini.cursorword').setup()
     lua require('mini.completion').setup()
     lua require('mini.statusline').setup()
     lua require('mini.surround').setup()
     lua require('mini.pairs').setup()
     lua require('mini.map').setup()
     lua require('nvim-lastplace').setup()
     lua require('nvim-web-devicons').setup()
     lua require('gitsigns').setup()
     lua require('ccc').setup()
    '';
  };

  postInstall = (drv.postInstall or "") + ''
    ln -sf "$vimrc" "$out/share/nvim/sysinit.vim"
  '';
})
