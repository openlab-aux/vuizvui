let
  aszligCollections = pkgs: let
    genAszligEnv = name: paths: pkgs.buildEnv {
      name = "aszlig-${name}-packages";
      inherit paths;
      ignoreCollisions = true;
    };
  in pkgs.lib.mapAttrsToList genAszligEnv {
    haskell = with pkgs.haskellPackages; [
      ghc cabalInstall hlint
      darcs
      diagrams
      yesod yesodStatic yesodDefault yesodTest
      hjsmin persistentSqlite
    ];

    python = with pkgs.pythonPackages; [
      pkgs.python3
      pkgs.python
      pep8
      polib
    ];

    shell = with pkgs; [
      zsh dash
      taskwarrior
      screen
      htop
      bc
      lftp
      mmv
      ncdu
      surfraw
      w3m
      fbida
      mutt
      vlock
    ];

    multimedia = with pkgs; [
      pulseaudio pvolctrl
      MPlayer
      vorbisTools
      mpg321
      mp3info
    ];

    crypto = with pkgs; [
      gnupg1compat openssh
      keychain
    ];

    dev = with pkgs; [
      gitFull
      gdb
      gnumake
      vim_configurable
      ltrace strace
    ];

    net = with pkgs; [
      netrw
      nmap
      socat
    ];

    x11 = with pkgs; [
      i3 i3lock i3status dmenu conky
      tkabber
      tkabber_plugins
    ];

    misc = with pkgs; [
      lastwatch
      chromiumBetaWrapper
      glxinfo
      imagemagick
      graphviz
      youtubeDL
      ghostscript
      rtorrent
    ];
  };

  tkabberRev = 2009;
in {
  pulseaudio = true;
  chromium.enableGoogleTalkPlugin = true;
  chromium.jre = true;

  firefox.jre = true;

  packageOverrides = pkgs: with pkgs; let p = {
    aszligEnv = (buildEnv {
      name = "aszlig-env";
      paths = aszligCollections (pkgs // p);
    });

    pulseaudio = pulseaudio.override {
      useSystemd = true;
    };

    kernelSourceAszlig = {
      version = "3.8.0-rc1";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "637704cbc95c02d18741b4a6e7a5d2397f8b28ce";
        sha256 = "0b80wls6mv741gnc0mqp959z5f98p3r78b8b0irni7im8bdvlx82";
      };
    };

    blop = stdenv.mkDerivation rec {
      name = "blop-${version}";
      version = "0.2.8";

      configureFlags = [
        "--with-ladspa-prefix=${ladspaH}"
        "--with-ladspa-plugin-dir=$(out)/lib/ladspa"
      ];

      src = fetchurl {
        url = "mirror://sourceforge/blop/${name}.tar.gz";
        sha256 = "02iymw84dml8glyqgx1mxq4fz2fifgi1jca28hx2r3a2mi7i71vy";
      };
    };

    libCMT = stdenv.mkDerivation rec {
      name = "libcmt-${version}";
      version = "1.16";

      buildInputs = [ ladspaH ];

      setSourceRoot = ''
        sourceRoot=cmt/src
      '';

      makeFlags = [
        "INSTALL_PLUGINS_DIR=$(out)/lib/ladspa"
      ];

      preInstall = ''
        mkdir -p "$out/lib/ladspa"
      '';

      src = fetchurl {
        url = "http://www.ladspa.org/download/cmt_src_${version}.tgz";
        sha256 = "0dan83pvljij3972bv214balc26p9fgw40i2d5y0x7lbd5z1saji";
      };
    };

    tkabber = lib.overrideDerivation tkabber (o: {
      name = "tkabber-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber";
        rev = tkabberRev;
        sha256 = "0lfh3bapqsfw142bndp11x7cs9crrcccw242lgwlh103r9gs123s";
      };
    });

    tkabber_plugins = lib.overrideDerivation tkabber_plugins (o: {
      name = "tkabber-plugins-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber-plugins";
        rev = tkabberRev;
        sha256 = "181jxd7iwpcl7wllwciqshzznahdw69fy7r604gj4m2kq6qmynqf";
      };
    });

    w3m = w3m.override {
      graphicsSupport = true;
    };

    netrw = netrw.override {
      checksumType = "mhash";
    };

    kernelEnv = myEnvFun {
      name = "kernel";
      extraCmds = ''
        export NIX_LDFLAGS="$NIX_LDFLAGS -lncurses"
      '';
      buildInputs = [
        stdenv ncurses
      ];
    };

    testChromiumBuild = let
      buildChannels = [ "stable" "beta" "dev" ];
      buildChromium = chan: chromium.override {
        channel = chan;
        gnomeSupport = true;
        gnomeKeyringSupport = true;
        proprietaryCodecs = true;
        cupsSupport = true;
        pulseSupport = true;
      };
    in stdenv.mkDerivation {
      name = "test-chromium-build";

      buildCommand = let
        chanResults = lib.flip map buildChannels (chan: ''
          echo "Build result for ${chan}: ${buildChromium chan}"
        '');
      in ''
        echo "Builds finished, the following derivations have been built:"
        ${lib.concatStrings chanResults}
        false
      '';
    };

    catapultEnv = myEnvFun {
      name = "catapult";
      buildInputs = [
        stdenv python pil
        pythonPackages.matplotlib
        pythonPackages.django
        pythonPackages.sqlite3
        pythonPackages.markdown
        pythonPackages.MySQL_python
        pythonPackages.setuptools
      ];
    };

    hetznerEnv = myEnvFun {
      name = "hetzner";
      buildInputs = [
        stdenv python
        pythonPackages.pexpect
      ];
    };

    rdwarfEnv = myEnvFun {
      name = "rdwarf";
      buildInputs = [
        stdenv python
        pythonPackages.numpy
        pythonPackages.pyaudio
        pythonPackages.curses
      ];
    };

    pvolctrl = stdenv.mkDerivation rec {
      name = "pvolctrl-0.23";

      unpackPhase = let
        baseurl = "https://sites.google.com/site/guenterbartsch/blog/"
                + "volumecontrolutilityforpulseaudio/";
        makefile = fetchurl {
          url = baseurl + "Makefile";
          sha256 = "0l2ffvb617csk6h29y64v6ywhpcp7la6vvcip1w4nq0yry6jhrqz";
        };
        source = fetchurl {
          url = baseurl + "pvolctrl.c";
          sha256 = "0vcd5dlw9l47jpabwmmzdvlkn67fz55dr3sryyh56sl263mibjda";
        };
      in ''
        mkdir -p "${name}"
        sed -e 's|/usr/bin/||' "${makefile}" > "${name}/Makefile"
        sed -e 's/PA_VOLUME_MAX/PA_VOLUME_NORM/
        /avg_vol += (avg_vol \* vol_mod) \/ 100;/ {
          s/(avg_vol/((int)PA_VOLUME_NORM/
        }
        /if (vol_mod)/i \
          if (info->name == NULL || strncmp(info->name, "combined", 8) != 0) \
            return;' "${source}" > "${name}/pvolctrl.c"
        sourceRoot="${name}"
      '';

      installPhase = ''
        install -D -T pvolctrl "$out/bin/pvolctrl"
      '';

      buildInputs = [ pkgconfig pulseaudio ];
    };

    axbo_research = stdenv.mkDerivation rec {
      name = "axbo-research-${version}";
      version = "2_0_18";

      unpackPhase = ''
        offset="$(sed -n 's/^ *tail *-c *\([0-9]\+\).*$/\1/p' "$src")"
        tail -c "$offset" "$src" | tar xz
        ls -la
        #${unzip}/bin/unzip i4jruntime.jar
        cat i4jparams.conf
      '';

      src = fetchurl {
        url = "http://www.axbo.com/webstart/aXbo_unix_${version}.sh";
        sha256 = "1zc3bpqfa5pdpl7masigvv98mi5phl04p80fyd2ink33xbmik70z";
      };
    };
  }; in p;
}
