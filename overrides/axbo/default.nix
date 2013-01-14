{ stdenv, fetchurl, jre, librxtx_java, makeFontsConf, dejavu_fonts }:

stdenv.mkDerivation rec {
  name = "axbo-research-${version}";
  version = "2.0.18";

  buildInputs = [ jre librxtx_java ];

  unpackCmd = let
    fontconfigFile = makeFontsConf {
      fontDirectories = stdenv.lib.singleton dejavu_fonts;
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
      upstream_version = stdenv.lib.replaceChars ["."] ["_"] version;
    in "http://www.axbo.com/webstart/aXbo_unix_${upstream_version}.sh";
    sha256 = "1zc3bpqfa5pdpl7masigvv98mi5phl04p80fyd2ink33xbmik70z";
  };
}
