{ pkgs ? import <nixpkgs> {} }:

pkgs.neovim-unwrapped.overrideAttrs (drv: {
  vimrc = pkgs.vimUtils.vimrcFile {
    packages.myplugins = with pkgs.vimPlugins; {
      start = [
        mini-nvim
        vim-nix
        vim-sleuth
        nvim-lastplace
        gitsigns-nvim
        zen-mode-nvim
        twilight-nvim
        nvim-web-devicons
      ];
      opt = [ ];
    };
    customRC = ''
     vim.opt.termguicolors = true
     vim.opt.tabstop = 4
     vim.opt.softtabstop = 4
     vim.opt.shiftwidth = 4
     vim.opt.expandtab = true
     vim.opt.autoindent = true
     vim.opt.smartindent = true
     vim.opt.colorcolumn = 80
     vim.opt.history = 500
     vim.opt.hlsearch = true
     vim.opt.incsearch = true
     vim.opt.nu = true
     vim.opt.showmatch = true
     vim.opt.smartcase = true
     vim.opt.smarttab = true
     vim.opt.signcolumn = "yes"

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
    '';
  };

  postInstall = (drv.postInstall or "") + ''
    ln -sf "$vimrc" "$out/share/nvim/sysinit.vim"
  '';

  meta = (drv.meta or {}) // {
    hydraPlatforms = [ "x86_64-linux" ];
  };
})
