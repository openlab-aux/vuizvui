{ pkgs ? import <nixpkgs> {} }:

let
  generic = ''
    syntax on
    colorscheme elflord

    " boolean
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

    " non-boolean
    set tabstop=4
    set softtabstop=4
    set shiftwidth=4
    set textwidth=80
    set termencoding=ascii
    set backspace=indent,eol,start
    set background=dark
  '';

  plugin = ''
    " erlang
    let erlang_folding = 0
    let erlang_highlight_bif = 1
    let erlang_force_use_vimerl_indent = 1

    " python
    let python_highlight_numbers = 1
    let python_highlight_builtins = 1
    let python_highlight_exceptions = 1
  '';

  autocmd = ''
    " jump to last position
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
                   \ exe "normal! g'\"zz" | endif

    " filetype defaults
    filetype plugin indent on
    au BufNewFile,BufRead *.as set filetype=actionscript
    au BufNewFile,BufRead *.html set tabstop=4|set shiftwidth=4|set expandtab
    au FileType python set textwidth=79
    au FileType gitcommit set textwidth=72

    " highlight unnecessary whitespace
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
    au BufWinEnter,InsertLeave * match ExtraWhitespace /\s\+$/
    au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    " prevent colorscheme from overriding these highlights
    au ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

    " highlight everything exceeding 80 characters
    au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
  '';

  misc = ''
    " ASCII art mode
    fun! AAMode()
      highlight clear ExtraWhitespace
      for m in getmatches()
        if m.group == 'ErrorMsg' && m.pattern == '\%>80v.\+'
          call matchdelete(m.id)
        endif
      endfor
    endfun

    " flake everything that has been *detected* as python (not just by suffix).
    autocmd BufWritePost * if &ft ==# 'python' | call Flake8() | endif
  '';

in pkgs.writeText "vimrc" ''
  ${generic}
  ${plugin}

  if has("autocmd")
    ${autocmd}
  endif

  ${misc}
''
