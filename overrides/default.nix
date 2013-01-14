pkgs:

with {
  tkabberRev = 2009;
};
with pkgs;

let
  callPackage = lib.callPackageWith (pkgs // self);
  self = rec {
    pulseaudio = pkgs.pulseaudio.override {
      useSystemd = true;
    };

    kernelSourceAszlig = {
      version = "3.8.0-rc3";
      src = pkgs.fetchgit {
        url = /home/aszlig/linux;
        rev = "ecf02a607bd801e742d7bb35c6e40f7ca15edf03";
        sha256 = "19ad666ms61s55730sh2n2lp9dsn439lnr7cypdma978mx27aj7v";
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

    tkabber = lib.overrideDerivation pkgs.tkabber (o: {
      name = "tkabber-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber";
        rev = tkabberRev;
        sha256 = "0lfh3bapqsfw142bndp11x7cs9crrcccw242lgwlh103r9gs123s";
      };
    });

    tkabber_plugins = lib.overrideDerivation pkgs.tkabber_plugins (o: {
      name = "tkabber-plugins-1.0pre";
      src = fetchsvn {
        url = "http://svn.xmpp.ru/repos/tkabber/trunk/tkabber-plugins";
        rev = tkabberRev;
        sha256 = "181jxd7iwpcl7wllwciqshzznahdw69fy7r604gj4m2kq6qmynqf";
      };
    });

    w3m = pkgs.w3m.override {
      graphicsSupport = true;
    };

    netrw = pkgs.netrw.override {
      checksumType = "mhash";
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

    librxtx_java = stdenv.mkDerivation rec {
      name = "rxtx-${version}";
      version = "2.1-7r2";

      buildInputs = [ unzip jdk ];

      NIX_CFLAGS_COMPILE = "-DUTS_RELEASE=\"3.8.0\"";

      makeFlags = [
        "JHOME=$(out)/lib/java"
        "RXTX_PATH=$(out)/lib"
      ];

      preInstall = ''
        mkdir -p "$out/lib/java"
      '';

      src = fetchurl {
        url = "http://rxtx.qbang.org/pub/rxtx/${name}.zip";
        sha256 = "1nfxdbiamr8dmls4zbdcdk4hf916gnr1jmcpb1kpc1b1m193inri";
      };
    };

    axbo_research = stdenv.mkDerivation rec {
      name = "axbo-research-${version}";
      version = "2.0.18";

      buildInputs = [ jre librxtx_java ];

      unpackCmd = let
        fontconfigFile = makeFontsConf {
          fontDirectories = lib.singleton dejavu_fonts;
        };
      in ''
        datalen="$(sed -n 's/^.*totalDataLength=\([0-9]\+\).*$/\1/p' "$src")"
        installer_offset="$(sed -n 's/^ *tail *-c *\([0-9]\+\).*$/\1/p' "$src")"

        installer_dir="$(mktemp -d)"
        mkdir -p "$installer_dir"
        tail -c "$installer_offset" "$src" | tar xz -C "$installer_dir"

        cat > "$installer_dir/responses" <<EOF
        executeLauncherAction$Boolean=false
        sys.programGroup.linkDir=/dev/null
        sys.component.73$Boolean=true
        sys.languageId=en
        sys.installationDir=$(pwd)/${name}
        sys.programGroup.enabled$Boolean=false
        sys.programGroup.allUsers$Boolean=true
        sys.programGroup.name=aXbo
        EOF

        cd "$installer_dir"
        export FONTCONFIG_FILE="${fontconfigFile}"
        java -client -Dinstall4j.jvmDir="${jre}" \
                     -Dexe4j.moduleName="$src" \
                     -Dexe4j.totalDataLength="$datalen" \
                     -Dinstall4j.cwd="$installer_dir" \
                     -Djava.ext.dirs="${jre}/lib/ext" \
                     -Dsun.java2d.noddraw=true \
                     -classpath i4jruntime.jar:user.jar \
                     com.install4j.runtime.Launcher launch \
                     com.install4j.runtime.installer.Installer \
                     false false "" "" false true false "" true true \
                     0 0 "" 20 20 Arial 0,0,0 8 500 'version 2.0.18' \
                     20 40 Arial 0,0,0 8 500 \
                     -1 -q -varfile "$installer_dir/responses"
        cd -
        rm -rf "$installer_dir"
      '';

      installPhase = ''
        mkdir -p "$out/lib/java" "$out/libexec" "$out/bin"
        for jarfile in lib/*; do
          case "''${jarfile##*/}" in
            axbo.jar) cp -vt "$out/libexec" "$jarfile";;
            RXTXcomm.jar) ;; # ignore
            *.jar) cp -vt "$out/lib/java" "$jarfile";;
          esac
        done

        cat > "$out/bin/axbo-research" <<WRAPPER
        #!${stdenv.shell}
        ${jre}/bin/java -Djava.library.path="${librxtx_java}/lib" \
          -classpath "${librxtx_java}/lib/java/*:$out/lib/java/*" \
          -jar "$out/libexec/axbo.jar"
        WRAPPER
        chmod +x "$out/bin/axbo-research"
      '';

      src = fetchurl {
        url = let
          upstream_version = lib.replaceChars ["."] ["_"] version;
        in "http://www.axbo.com/webstart/aXbo_unix_${upstream_version}.sh";
        sha256 = "1zc3bpqfa5pdpl7masigvv98mi5phl04p80fyd2ink33xbmik70z";
      };
    };
  };
in self
