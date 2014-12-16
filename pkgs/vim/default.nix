{ stdenv, lib, writeTextFile, writeText, buildEnv
, fetchurl, fetchFromGitHub, vim_configurable
}:

let
  fetchVimScript = { srcId, sha256, type, name }: let
    baseUrl = "http://www.vim.org/scripts/download_script.php";
    src = fetchurl {
      name = "script${toString srcId}.vim";
      url = "${baseUrl}?src_id=${toString srcId}";
      inherit sha256;
    };
  in stdenv.mkDerivation {
    name = "vim-${type}-${toString srcId}";
    buildCommand = ''
      install -vD -m 0644 "${src}" "$out/${type}/${name}.vim"
    '';
  };

  extractSubdir = subdir: src: stdenv.mkDerivation {
    name = "${src.name}-subdir";
    phases = [ "unpackPhase" "installPhase" ];
    inherit src;
    installPhase = ''
      cp -Rd "${subdir}" "$out"
    '';
  };

  mkVimPlugins = plugins: buildEnv {
    name = "vim-plugins";
    paths = with lib; mapAttrsToList (const id) plugins;
    ignoreCollisions = true;
    postBuild = ''
      find -L "$out" -mindepth 1 -maxdepth 1 -type f -delete
    '';
  };

  pluginDeps = {
    vimAddonMwUtils = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-mw-utils";
      rev = "0c5612fa31ee434ba055e21c76f456244b3b5109";
      sha256 = "147s1k4n45d3x281vj35l26sv4waxjlpqdn83z3k9n51556h1d45";
    };

    vimAddonCompletion = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-completion";
      rev = "80f717d68df5b0d7b32228229ddfd29c3e86e435";
      sha256 = "08acffzy847w8b5j8pdw6qsidm2859ki5q351n4r7fkr969p80mi";
    };

    vimAddonActions = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-actions";
      rev = "a5d20500fb8812958540cf17862bd73e7af64936";
      sha256 = "1wfkwr89sn2w97i94d0dqylcg9mr6pirjadi0a4l492nfnsh99bc";
    };

    vimAddonBackgroundCmd = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-background-cmd";
      rev = "14df72660a95804a57c02b9ff0ae3198608e2491";
      sha256 = "09lh6hqbx05gm7njhpqvhqdwig3pianq9rddxmjsr6b1vylgdgg4";
    };

    vimAddonErrorFormats = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-errorformats";
      rev = "dcbb203ad5f56e47e75fdee35bc92e2ba69e1d28";
      sha256 = "159zqm69fxbxcv3b2y99g57bf20qrzsijcvb5rzy2njxah3049m1";
    };

    vimAddonToggleBuffer = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-toggle-buffer";
      rev = "a1b38b9c5709cba666ed2d84ef06548f675c6b0b";
      sha256 = "1xq38kfdm36c34ln66znw841q797w5gm8bpq1x64bsf2h6n3ml03";
    };

    vimAddonGotoThingAtCursor = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-goto-thing-at-cursor";
      rev = "f052e094bdb351829bf72ae3435af9042e09a6e4";
      sha256 = "1ksm2b0j80zn8sz2y227bpcx4jsv76lwgr2gpgy2drlyqhn2vlv0";
    };

    vimAddonViews = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-views";
      rev = "d1383ad56d0a07d7350880adbadf9de501729fa8";
      sha256 = "09gqh7w5rk4lmra706schqaj8dnisf396lpsipm7xv6gy1qbslnv";
    };

    vimAddonSwfMill = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-swfmill";
      rev = "726777e02cbe3ad8f82e37421fb37674f446a148";
      sha256 = "0ablzl5clgfzhzwvzzbaj0cda0b4cyrj3pbv02f26hx7rfnssaqm";
    };

    vimHaxeSyntax = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-haxe-syntax";
      rev = "500acc2f2ab92d77ff6cd04fdc7868800c033dfa";
      sha256 = "1ipm0igplplfmscm3bk95qpf9rw71h133l9shmw54mxr4h0ymnmj";
    };

    tlib = fetchFromGitHub {
      owner = "tomtom";
      repo = "tlib_vim";
      rev = "bc4097bd38c4bc040fe1e74df68dec6c9adfcb6a";
      sha256 = "19v7bgmkk4k2g1z62bd0kky29xxfq96l7wfrl27wb2zijlhbrnpz";
    };

    vamStub = writeTextFile {
      name = "vam-stub";
      destination = "/autoload/vam.vim";
      text = ''
        fun! vam#DefineAndBind(local, global, default)
          return ' if !exists('.string(a:global).') |
                 \ let '.a:global.' = '.a:default.' |
                 \ endif | let '.a:local.' = '.a:global
        endfun
      '';
    };
  };

  plugins = mkVimPlugins (pluginDeps // {
    vimErl = fetchFromGitHub {
      owner = "jimenezrick";
      repo = "vimerl";
      rev = "823bf8cb515bb10396c705cdc017aa9121cc4d12";
      sha256 = "0sybkx8iy8qhr6nlwn52j7zd5v99rn0b8wbg43d112z2px4yq5x3";
    };

    vimHaxe = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-haxe";
      rev = "8efc705db41a01713d67d437f29866a1ff831e8a";
      sha256 = "15kv13gvpgf23p0566qrlw7gmpir2z7g5dnkfs1knmcwzw45am5d";
    };

    factor = extractSubdir "misc/vim" (fetchFromGitHub {
      owner = "slavapestov";
      repo = "factor";
      rev = "0d6f70cc7cf35cc627ee78886e2932091a651fe6";
      sha256 = "0lmqzvrmwgmxpcpwgn59y033sf4jybmw3lffbjwww5d7ch90333q";
    });

    opaLang = extractSubdir "tools/editors/vim" (fetchFromGitHub {
      owner = "MLstate";
      repo = "opalang";
      rev = "94e4e6d9d8da9a72214f4f28dd1ffa1a987997eb";
      sha256 = "0d6b67868cfqakkz63y5ynpz549lbpfzc3c3x7kx3ffsv10xy3bb";
    });

    lslvim = fetchFromGitHub {
      owner = "sukima";
      repo = "LSLvim";
      rev = "f269de39a1c713a43470e90d0ec78208c0f05e0b";
      sha256 = "1plwx5id3jsj4y6yhshlf3rishxhf1b9k47g2cpzaczvqb5bl40w";
    };

    vimSyntaxShakespeare = fetchFromGitHub {
      owner = "pbrisbin";
      repo = "vim-syntax-shakespeare";
      rev = "29085ae94ee3dbd7f39f2a7705d86692ef5bc365";
      sha256 = "0kvys81jiwqzwmpbk1lvbciw28yha4shd1xby5saiy4b68l6d8rk";
    };

    glsl = fetchVimScript {
      name = "glsl";
      srcId = 3194;
      sha256 = "1vqfcpjmfyjc95wns3i84kgd1k5r2lwjjvjcprygi9g9vng7i5xc";
      type = "syntax";
    };

    actionScript = fetchVimScript {
      name = "actionscript";
      srcId = 1205;
      sha256 = "0pdzqg678lhn7lmqf3z9icpj6ff2nnghsxy983kxkn8sblnzlhfs";
      type = "syntax";
    };

    indentPython = fetchVimScript {
      name = "python";
      srcId = 4316;
      sha256 = "1pgdiaqd1hm0qpspy1asj7i103pq0846lnjrxvl6pk17ymww9pmk";
      type = "indent";
    };

    nixAddon = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-nix";
      rev = "7b0a376bb1797fef8da2dc14e768f318bcb671e8";
      sha256 = "00dsba2a0az9bw1sni12km1vdga9x32dapxaj5gmq98hzqlhklyp";
    };

    urwebAddon = fetchFromGitHub {
      owner = "MarcWeber";
      repo = "vim-addon-urweb";
      rev = "49ea3960a9924a5dd7ff70956d1a7c0479a55773";
      sha256 = "090ww8nxqsabrwf4r8g7a93kawnp6zwpsx65yxpacwwwlbc73m7s";
    };

    indentHaskell = fetchVimScript {
      name = "haskell";
      srcId = 7407;
      sha256 = "1lj44jkyihmcnj2kcfckhqzr9gfipda9frbzicix2wrc5728kjsv";
      type = "indent";
    };

    fishSyntax = fetchVimScript {
      name = "fish";
      srcId = 20242;
      sha256 = "12gfmyxxf84f19bp8xfmkb9phbfkifn89sjgi8hnv6dn0a5y1zpj";
      type = "syntax";
    };

    elmVim = fetchFromGitHub {
      owner = "lambdatoast";
      repo = "elm.vim";
      rev = "ad556c97e26072b065825852ceead0fe6a1f7d7c";
      sha256 = "19k6b6m5ngm5qn2f3p13hzjyvha53fpdgq691z8n0lwfn8831b21";
    };

    flake8 = fetchFromGitHub {
      owner = "nvie";
      repo = "vim-flake8";
      rev = "293613dbe731a2875ce93739e7b64ee504d8bbab";
      sha256 = "0xmqmbh66g44vhx9769mzs820k6ksbpfnsfvivmbhzlps2hjqpqg";
    };
  });

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

    " all plugins
    set runtimepath^=${plugins}
    set runtimepath+=${plugins}/after
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

  vimrc = writeText "vimrc" ''
    ${generic}
    ${plugin}

    if has("autocmd")
      ${autocmd}
    endif

    ${misc}
  '';

in lib.overrideDerivation vim_configurable (o: {
  postInstall = (o.postInstall or "") + ''
    ln -sf "${vimrc}" "$out/share/vim/vimrc"
  '';
})
