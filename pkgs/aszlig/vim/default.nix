{ stdenv, lib, fetchurl, fetchFromGitHub, writeText, writeTextFile, writeScript
, runCommand, writers, python3Packages, ledger, meson, vim, buildGoModule
, rustfmt, ansifilter
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
      rev = "70c4e222464020edc2809c932b488daaf891eeef";
      sha256 = "1amx220nbh1s51z35pkhvl3110pbha5qj2rdgxvg8dbqha7py9fx";
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
      postFetch = ''
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
        rev = "048c71f1ed2c679cd55acd2c807c2c96aea82e65";
        hash = "sha256-IyLKu5sfD7RNHwI8kodQB4f1xKBLSEnnw6fBu3Zs5eg";
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

    fishSyntax = fetchFromGitHub {
      owner = "georgewitteman";
      repo = "vim-fish";
      rev = "667523b8fc0310b3f09492186a1e25b978ef8e5f";
      sha256 = "1ni9bkrc2m1y7sirp57piqks3ns4yin7hi4x71pjmyrdkqnv8xr1";
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
      postFetch = ''
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
      sha256 = "0g1qrq4f4gq0yrnlslrjz03m0997rd343b22w21w7359hydlqzax";
      postFetch = ''
        mv -t "$out" "$out/org.ats-lang.toolats.vim/ftdetect" \
                     "$out/org.ats-lang.toolats.vim/syntax"
        rm -rf "$out/org.ats-lang.toolats"* \
               "$out/README.md" "$out/release" "$out/.gitignore"
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

    jinja2 = fetchFromGitHub {
      owner = "Glench";
      repo = "Vim-Jinja2-Syntax";
      rev = "2c17843b074b06a835f88587e1023ceff7e2c7d1";
      sha256 = "13mfzsw3kr3r826wkpd3jhh1sy2j10hlj1bv8n8r01hpbngikfg7";
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

    hexokinase = buildGoModule {
      name = "hexokinase";
      outputs = [ "out" "bin" ];
      vendorHash = null;
      modRoot = "hexokinase";
      src = fetchFromGitHub {
        owner = "RRethy";
        repo = "vim-hexokinase";
        rev = "62324b43ea858e268fb70665f7d012ae67690f43";
        sha256 = "1qdy028i9zrldjx24blk5im35lcijvq4fwg63ks2vrrvn0dfsj01";
        fetchSubmodules = true;
      };
      doCheck = false;
      postPatch = ''
        # We don't want to highlight 0xaabbcc, only #aabbcc.
        sed -i -e 's/|0x)/)/g' hexokinase/hex.go

        sed -i -n -e '/^let g:Hexokinase_executable_path/ {
          :l; n; /^ *\\/bl
          i let g:Hexokinase_executable_path = '"'$bin/bin/hexokinase'"'
        }' -e p plugin/hexokinase.vim
      '';
      installPhase = ''
        install -vD "$GOPATH/bin/hexokinase" "$bin/bin/hexokinase"
        mkdir "$out"
        for i in autoload doc plugin; do
          cp -Rd "../$i" "$out"
        done
      '';
    };

    fluent = fetchFromGitHub {
      owner = "projectfluent";
      repo = "fluent.vim";
      rev = "2278e05ec7fbb48e06b5d26319385e1c09325760";
      sha256 = "0hp8jjr4xpw73pkfpbxpjnr49cvjyksymvj748zaxjznkvizmyxc";
    };

    godot = fetchFromGitHub {
      owner = "habamax";
      repo = "vim-godot";
      rev = "e38845b4042d2351c47cd63f8705fd51c97acb4f";
      sha256 = "1k3val0ibriwcv5jdyq95sgxgkz54r15gpylbhns5934zvaakpj1";
    };

    # Remove once https://github.com/rust-lang/rust.vim/issues/446 is resolved.
    rust = fetchFromGitHub {
      owner = "rust-lang";
      repo = "rust.vim";
      rev = "4aa69b84c8a58fcec6b6dad6fe244b916b1cf830";
      sha256 = "07nh8gvkwq91i7qcz0rk5jlc8sb4d3af4zq2892kmmw576zg1wd8";
    };

    quick-scope = fetchFromGitHub {
      owner = "unblevable";
      repo = "quick-scope";
      rev = "428e8698347f254d24b248af9f656194a80081e5";
      sha256 = "0vindr83v4q26a7jxfwk87vpl1kymsh6cclhvpkmb6cpq0iv3yii";
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
    set backspace=indent,eol,start
    set background=dark
    set mouse=
    set history=500
  '';

  vimLanguages = runCommand "vim-languages.json" {
    inherit vim;
    plugins = lib.attrValues plugins;
  } ''
    result=

    add_synfiles() {
      for synfile in "$@"; do
        synbase="$(basename "$synfile")"
        result="$result''${result:+,}\"''${synbase%.vim}\""
      done
    }

    add_synfiles "$vim"/share/vim/vim[0-9]*/syntax/*.vim
    for plugin in $plugins; do
      add_synfiles "$plugin"/syntax/*.vim
    done

    echo "[$result]" > "$out"
  '';

  githubLanguages = let
    rev = "16c70aef8cd62ca071231a380c69050f5e83c900";
    sha256 = "1bzxvf9128dzwc3n6hamvqld1idmagx0mfw8ydg4v7789yywppjj";
    file = "lib/linguist/languages.yml";
    url = "https://github.com/github/linguist/raw/${rev}/${file}";
  in fetchurl { inherit url sha256; };

  blacklistedLanguages = [
    # Support file rather than real syntax files, which we never ever want to
    # include directly.
    "syncolor" "synload" "syntax"

    # Those have poor runtime performance because they include other syntax
    # files. While there are certainly others, I excluded the following syntax
    # files because I find them highly unlikely to occur in code blocks from
    # Markdown files I care about.
    "2html" "ant" "antlr" "groovy" "gsp" "haml" "javacc" "jinja" "jsp" "less"
    "lucius" "phtml" "rpcgen" "sass" "scss" "smarty" "sqlj" "vim" "vue" "xs"

    # Cause conflicts because they execute "syn sync linecont".
    "fortran" "rexx" "sicad"

    # These syntax files recursively include markdown.vim syntax file.
    "lhaskell" "markdown" "pug" "rmd"

    # Other markup languages I think are pretty unlikely to be included in
    # Markdown code blocks.
    "asciidoc" "dircolors" "docbk" "docbksgml" "docbkxml" "doxygen" "groff"
    "nroff" "rst" "rtf"
  ];

  transformer = writers.writePython3Bin "transform-languages" {
    libraries = lib.singleton python3Packages.pyyaml;
    flakeIgnore = [ "E111" "E114" "E501" ];
  } ''
    import json
    import sys
    import yaml

    blacklisted_languages = {${
      lib.concatMapStringsSep ", " (l: "'${l}'") blacklistedLanguages
    }}

    with open(sys.argv[1]) as fp:
      vim_languages = set(json.load(fp)) - blacklisted_languages

    # This generate language candidates/aliases according to
    # https://git.io/Jcioq with the keys being the corresponding Vim syntax
    # file name and the values being a set of all the aliases we can use for
    # GitHub Flavored Markdown.
    languages = {}
    with open(sys.argv[2]) as fp:
      for name, lang in yaml.load(fp, Loader=yaml.CLoader).items():
        if lang['tm_scope'] == 'none':
          continue

        aliases = lang.get('aliases', [])
        interpreters = lang.get('interpreters', [])
        # The Vim variant doesn't support a dot prefix to the language name.
        extensions = [ext.lstrip('.') for ext in lang.get('extensions', [])]

        raw_candidates = [name] + aliases + interpreters + extensions
        # Note that this is not a set (yet), because order is important here,
        # going from the most specific ones to the least specific ones.
        candidates = [c.replace(' ', '-').lower() for c in raw_candidates]

        for candidate in candidates:
          if candidate in vim_languages:
            # At this point we *do* want to make sure it's a set because we
            # only want to provide the aliases once.
            languages[candidate] = set(candidates)
            break

    # This is for getting the candidates/aliases into the format expected by
    # g:markdown_fenced_languages, which either is the syntax file directly or
    # some aliased mapping like "bash=sh".
    fenced_langs = []
    for name, candidates in languages.items():
      fenced_langs += [c if c == name else f'{c}={name}' for c in candidates]

    with open(sys.argv[3], 'w') as fp:
      escaped = ["'" + val.replace("'", "'''") + "'" for val in fenced_langs]
      vim_fenced_langs = '[' + ', '.join(escaped) + ']'
      fp.write('let g:markdown_fenced_languages = ' + vim_fenced_langs + "\n")
  '';

  vimMarkdownLanguages = runCommand "markdown-languages.vim" {
    inherit vimLanguages githubLanguages;
    nativeBuildInputs = lib.singleton transformer;
  } "transform-languages \"$vimLanguages\" \"$githubLanguages\" \"$out\"";

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

    " markdown
    source ${vimMarkdownLanguages}

    " Luckily I rarely have to do with Java stuff, so to prevent recursion with
    " Markdown syntax, let's just disable Markdown for Java.
    let g:java_ignore_markdown = 1

    " hexokinase
    let g:Hexokinase_highlighters = ['background']
    let g:Hexokinase_refreshEvents =
      \ ['TextChanged', 'TextChangedI', 'InsertLeave', 'BufRead']
    let g:Hexokinase_optInPatterns =
      \ ['full_hex', 'rgb', 'rgba', 'hsl', 'hsla']

    " rust
    let g:rustfmt_command = '${rustfmt.override {
      asNightly = true;
    }}/bin/rustfmt'
    let g:rustfmt_autosave_if_config_present = 1
    let g:rust_recommended_style = 0
    let g:rustfmt_detect_version = 1
    let g:rustfmt_find_toml = 1

    packadd! editorconfig

    " disable highlighting line lengths, because we already handle it ourselves
    let g:EditorConfig_max_line_indicator = "fillexceeding"
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
    au FileType gitcommit let b:EditorConfig_disable = 1
    au FileType diff let b:EditorConfig_disable = 1
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

    " highlight everything exceeding textwidth characters (with exceptions)
    au BufWinEnter * if index(['diff', 'csv', 'strace', 'xt'], &ft) < 0
      \ | let w:m2=matchadd('ErrorMsg', '\%>' . &textwidth . 'v.\+', -1)
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
    set termguicolors

    ${functions}

    if has("autocmd")
      ${autocmd}
    endif
  '';


  installPlugin = name: plugin: let
    mkInst = targetDir: writeScript "install-plugin-file" ''
      #!${stdenv.shell}
      if [ -e "${targetDir}/$1" ]; then
        cat "$1" >> "${targetDir}/$1"
      else
        exec install -m 0644 -vD "$1" "${targetDir}/$1"
      fi
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

  # Fix elflord color theme to use the 16 color terminal colors in GUI mode as
  # well for consistence. Also, I'm already used to the colors and I don't for
  # example like the "Statement" guifg color.
  colorscheme = stdenv.mkDerivation {
    name = "elflord.vim";

    src = fetchFromGitHub {
      owner = "vim";
      repo = "colorschemes";
      rev = "2c1ab01646baa29a8908b674d6178e3c7fd5b25c";
      hash = "sha256-4jcjyv9BPCW3QFZrM5xvkwnBwu+OINcnraEigr1cowQ";
    };

    patches = [ ./elflord.patch ];

    colortemplate = fetchFromGitHub {
      owner = "lifepillar";
      repo = "vim-colortemplate";
      rev = "v2.2.0";
      hash = "sha256-DAdMvhRkvb+KA9/mYygvVBBfGEYNURspvRtvu9/WxoI";
    };

    nativeBuildInputs = [ vim ansifilter ];

    buildPhase = ''
      rm -r colors
      vim --not-a-term --clean -n --cmd "set rtp+=$colortemplate" \
        -c "Colortemplate! generated" -c 'qall' \
        colortemplate/elflord.colortemplate > vim-build.log 2>&1
      if [ ! -e colors/elflord.vim ]; then
        ansifilter vim-build.log >&2
        exit 1
      fi
    '';

    installPhase = ''
      cp colors/elflord.vim "$out"
    '';
  };

in lib.overrideDerivation vim (drv: {
  patchPhase = (drv.patchPhase or "") + ''
    cp ${lib.escapeShellArg colorscheme} runtime/colors/elflord.vim
  '';

  postInstall = (drv.postInstall or "") + ''
    export vimdir="$(echo "$out/share/vim/vim"[0-9]*)"
    ${lib.concatStrings (lib.mapAttrsToList installPlugin plugins)}
    ln -sf "${vimrc}" "$out/share/vim/vimrc"
  '';
})
