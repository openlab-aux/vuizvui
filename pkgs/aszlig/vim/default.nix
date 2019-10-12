{ stdenv, lib, fetchurl, fetchFromGitHub, writeText, writeTextFile, writeScript
, python3Packages, ledger, meson, vim
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

    pug = fetchFromGitHub {
      owner = "digitaltoad";
      repo = "vim-pug";
      rev = "ddc5592f8c36bf4bd915c16b38b8c76292c2b975";
      sha256 = "069pha18g1nlzg44k742vjxm4zwjd1qjzhfllkr35qaiflvjm84y";
    };

    scss = fetchFromGitHub {
      owner = "cakebaker";
      repo = "scss-syntax.vim";
      rev = "4461789d02f81fd328afbdf27d6404b6c763c25f";
      sha256 = "0d227d2c1pvcksk2njzpkgmxivrnfb0apn2r62q7q89s61ggbzfj";
    };

    less = fetchFromGitHub {
      owner = "groenewege";
      repo = "vim-less";
      rev = "6e818d5614d5fc18d95a48c92b89e6db39f9e3d6";
      sha256 = "0rhqcdry8ycnfbg534q4b3hm78an7mnqhiazxik7k08a57dk9dbm";
    };
  };

  plugins = pluginDeps // {
    vaxe = fetchFromGitHub {
      owner = "jdonaldson";
      repo = "vaxe";
      rev = "d5f905f806c7c90bb116d4b06a78924341840021";
      sha256 = "0axvavzxbi3m4shva1m0cm6finl1i2rwqgn6lnklxnr2g9sfi4j7";
      extraPostFetch = ''
        # Do not highlight ',' and ';'.
        sed -i -e '/\<haxeOperator2\>/d' "$out/syntax/haxe.vim"
      '';
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

    nixAddon = stdenv.mkDerivation {
      name = "vim-nix-support";

      lnl7 = fetchFromGitHub {
        owner = "LnL7";
        repo = "vim-nix";
        rev = "be0c6bb409732b79cc86c177ca378b0b334e1efe";
        sha256 = "1ivkwlm6lz43xk1m7aii0bgn2p3225dixck0qyhxw4zxhp2xiz06";
      };

      src = fetchFromGitHub {
        owner = "MarcWeber";
        repo = "vim-addon-nix";
        rev = "3001a9db5f816dd7af11384f15415bddd146ef86";
        sha256 = "195z2yz09wirpqjpsha8x7qcr9is1q8qph4j0svws6qbqrkh8ryy";
      };

      phases = [ "unpackPhase" "patchPhase" "installPhase" ];
      patchPhase = ''
        for what in indent syntax; do
          install -vD -m 0644 "$lnl7/$what/nix.vim" "$what/nix.vim"
        done

        sed -i -re '/^ *au(group)? /,/^ *au(group)? +end/ {
          s/^ *au(tocmd)? +((BufRead|BufNewFile),?)+ +[^ ]+ +setl(ocal)?/${
            "& sw=2 sts=2 et iskeyword+='\\''"
          }/
        }' plugin/vim-addon-nix.vim

        sed -n -e '/^ *setlocal/ {
          h; :l; $ { x; p; b }; n; /^ *\\/ { H; bl }; x; p
        }' "$lnl7/ftplugin/nix.vim" >> ftplugin/nix.vim
      '';

      installPhase = ''
        cp -Rd . "$out"
      '';
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

    vader = fetchFromGitHub {
      owner = "junegunn";
      repo = "vader.vim";
      rev = "ad2c752435baba9e7544d0046f0277c3573439bd";
      sha256 = "0yvnah4lxk5w5qidc3y5nvl6lpi8rcv26907b3w7vjskqc935b8f";
    };

    multipleCursors = fetchFromGitHub {
      owner = "terryma";
      repo = "vim-multiple-cursors";
      rev = "3afc475cc64479a406ce73d3333df1f67db3c73f";
      sha256 = "04dijb4hgidypppphcy83bacmfrd9ikyjc761hqq6bl4kc49f5kc";
    };

    csv = fetchFromGitHub {
      owner = "chrisbra";
      repo = "csv.vim";
      rev = "443fa8bd2a1a017b26cc421a9494e1a1e33f4acf";
      sha256 = "1pbgl9f00kqxr2dpxmxg9jnk5q41sxzgan7hn16hc2b4as3zbihd";
      extraPostFetch = ''
        # Use sane (non-UTF8) settings for separators
        sed -i -e 's/(&enc *[=~#]\+ *.utf-8. *?[^:]*: *\([^)]*\))/\1/g' \
          "$out/ftplugin/csv.vim" "$out/syntax/csv.vim"
      '';
    };

    sleuth = fetchFromGitHub {
      owner = "tpope";
      repo = "vim-sleuth";
      rev = "dfe0a33253c61dd8fac455baea4ec492e6cf0fe3";
      sha256 = "0576k4l2wbzy9frvv268vdix4k6iz9pw6n6626ifvg8hk6gbc5g9";
    };

    ats = fetchFromGitHub {
      owner = "alex-ren";
      repo = "org.ats-lang.toolats";
      rev = "e0c5499dfa5c65b4aa3bf031247c768f826f3de8";
      sha256 = "1wf8pr4pj660bxq00l9fhr07qm7mpy1jglmsyxzi9qq9pgb2avzy";
      extraPostFetch = ''
        mv -t "$out" "$out/org.ats-lang.toolats.vim/ftdetect" \
                     "$out/org.ats-lang.toolats.vim/syntax"
        rm -rf "$out/org.ats-lang.toolats.vim"
      '';
    };

    ledger = fetchFromGitHub {
      owner = "ledger";
      repo = "vim-ledger";
      rev = "6eb3bb21aa979cc295d0480b2179938c12b33d0d";
      sha256 = "0rbwyaanvl2bqk8xm4kq8fkv8y92lpf9xx5n8gw54iij7xxhnj01";
    };

    vue = fetchFromGitHub {
      owner = "posva";
      repo = "vim-vue";
      rev = "e531e1d24f24385a5f4d2f1ba36d972a57ec52d9";
      sha256 = "1vi4i9ybwg1l1xmarsdhzd08py4w0yfg4xswbz3qrvihk8nhg1km";
    };

    meson = stdenv.mkDerivation {
      name = "meson-vim-${meson.version}";
      inherit (meson) src;
      phases = [ "unpackPhase" "patchPhase" "installPhase" ];
      postPatch = ''
        sed -i -e '/^ *echom \+getline/d' \
          data/syntax-highlighting/vim/indent/meson.vim
      '';
      installPhase = "cp -r data/syntax-highlighting/vim \"$out\"";
    };

    jinja2 = stdenv.mkDerivation {
      name = "jinja2-vim-${python3Packages.jinja2.version}";
      inherit (python3Packages.jinja2) src;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        install -vD -m 0644 ext/Vim/jinja.vim "$out/syntax/jinja.vim"
      '';
    };

    xdebug = fetchurl {
      name = "vim-xt-syntax";
      url = "https://raw.githubusercontent.com/xdebug/xdebug/"
          + "ce4f6bc7ae04ae542960af6c1b8975888e9c3e5e/contrib/xt.vim";
      sha256 = "05a3nry310s2w1h2q7w6yw2wick81jrnrs43x9vk0k7dqyavhvhi";
      downloadToTemp = true;
      recursiveHash = true;
      postFetch = ''
        install -vD -m 0644 "$downloadedFile" "$out/syntax/xt.vim"
      '';
    };

    purescript = fetchFromGitHub {
      owner = "purescript-contrib";
      repo = "purescript-vim";
      rev = "67ca4dc4a0291e5d8c8da48bffc0f3d2c9739e7f";
      sha256 = "1insh39hzbynr6qxb215qxhpifl5m8i5i0d09a3b6v679i7s11i8";
    };
  };

  generic = ''
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
    set textwidth=79
    set termencoding=ascii
    set backspace=indent,eol,start
    set background=dark
    set mouse=
    set history=500
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
    let g:flake8_cmd = '${python3Packages.flake8}/bin/flake8'

    " ledger
    let g:ledger_bin = '${ledger}/bin/ledger'
    let g:ledger_date_format = '%Y-%m-%d'
    let g:ledger_maxwidth = 79
    let g:ledger_align_at = 73
    let g:ledger_default_commodity = 'EUR'
    let g:ledger_commodity_before = 0
    let g:ledger_commodity_sep = ' '
    let g:ledger_fold_blanks = 1

    " php
    let php_noShortTags = 1
    let php_sql_query = 1
    let php_baselib = 1
    let php_htmlInStrings = 1
    let g:PHP_vintage_case_default_indent = 1
  '';

  autocmd = ''
    " jump to last position
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") |
                   \ exe "normal! g'\"zz" | endif

    " filetype defaults
    au BufNewFile,BufRead *.as setlocal ft=actionscript
    au BufNewFile,BufRead *.tt setlocal ft=tt2html ts=2 sw=2 sts=2 et
    au BufNewFile,BufRead *.xt setlocal ft=xt foldlevel=4
    au BufNewFile,BufRead *.html setlocal ts=2 sw=2 sts=2 et
    au FileType python setlocal textwidth=79
    au FileType gitcommit setlocal textwidth=72
    au FileType docbk setlocal tabstop=2 shiftwidth=2 expandtab

    " Enable folding for Ledger files
    au FileType ledger set fdm=syntax

    " Do not sleuth these file types!
    au FileType ledger let g:sleuth_automatic = 0
    au FileType haskell let g:sleuth_automatic = 0
    au FileType nix let g:sleuth_automatic = 0

    " highlight unnecessary whitespace
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
    au BufWinEnter,InsertLeave * match ExtraWhitespace /\s\+$/
    au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    " prevent colorscheme from overriding these highlights
    au ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

    " highlight everything exceeding 79 characters (with exceptions)
    au BufWinEnter * if index(['csv', 'strace', 'xt'], &ft) < 0
      \ | let w:m2=matchadd('ErrorMsg', '\%>79v.\+', -1)
      \ | endif

    " flake everything that has been *detected* as python (not just by suffix)
    au BufWritePost * if &ft ==# 'python' | call Flake8() | endif
  '';

  functions = ''
    " ASCII art mode
    fun! AAMode()
      highlight clear ExtraWhitespace
      for m in getmatches()
        if m.group ==# 'ErrorMsg' && m.pattern ==# '\%>79v.\+'
          call matchdelete(m.id)
        endif
      endfor
    endfun

    command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
      \ | wincmd p | diffthis
  '';

  vimrc = writeText "vimrc" ''
    let g:skip_defaults_vim = 1
    ${generic}
    ${plugin}

    " has to be after the generic block and before the autocmd block
    filetype plugin indent on
    syntax on
    colorscheme elflord

    ${functions}

    if has("autocmd")
      ${autocmd}
    endif
  '';


  installPlugin = name: plugin: let
    mkInst = targetDir: writeScript "install-plugin-file" ''
      #!${stdenv.shell}
      exec install -m 0644 -vD "$1" "${targetDir}/$1"
    '';

    afterPath = "$out/share/vim/vimfiles";

    findCmd = [
      "find" "-L" "." "-mindepth" "2" "-type" "f"
      "("  "-path" "*/after/*" "-exec" (mkInst afterPath) "{}" ";"
      "-o" "-exec" (mkInst "$vimdir") "{}" ";"
      ")"
    ];

  in ''
    ( cd ${lib.escapeShellArg plugin}
      ${lib.concatMapStringsSep " " lib.escapeShellArg findCmd}
    )
  '';

in lib.overrideDerivation vim (o: {
  postInstall = (o.postInstall or "") + ''
    export vimdir="$(echo "$out/share/vim/vim"[0-9]*)"
    ${lib.concatStrings (lib.mapAttrsToList installPlugin plugins)}
    ln -sf "${vimrc}" "$out/share/vim/vimrc"
  '';
})
