{ stdenv, fetchurl, fetchFromGitHub, jdk, jre, ant, makeWrapper
, commonsLogging, librxtx_java
}:

stdenv.mkDerivation rec {
  name = "axbo-research-${version}";
  version = "3.0.12";

  src = fetchFromGitHub {
    owner = "jansolo";
    repo = "aXbo-research";
    #rev = "aXbo-research_${version}";
    # Includes MIT license:
    rev = "6e6888917b5f200a44509650d6f46ec42c133cdc";
    sha256 = "0nbyxajl75q80cnyl9c0sjlyk3rmhm7k8w8mksg4lfyh78ynayyc";
  };

  sourceRoot = "${src.name}/aXbo-research";

  buildInputs = [ jdk ant makeWrapper ];

  buildPhase = ''
    ant -Dplatforms.JDK_1.7.home="$JAVA_HOME" jar
  '';

  extraJars = [
    "commons-beanutils-1.8.3"
    "commons-digester3-3.2"
    "commons-io-2.4"
    "jcommon-1.0.20"
    "jfreechart-1.0.16"
    "streamflyer-core-1.0.1"
    "swingx-all-1.6.4"
  ];

  installPhase = with stdenv.lib; let
    classpath = makeSearchPath "share/java/\\*" [
      "$out"
      commonsLogging
      librxtx_java
    ];
  in ''
    for dep in $extraJars; do
      install -vD -m 644 "lib/$dep.jar" "$out/share/java/$dep.jar"
    done
    install -vD -m 644 dist/axbo.jar "$out/share/java/axbo.jar"

    mkdir -p "$out/bin"
    makeWrapper "${jre}/bin/java" "$out/bin/axbo-research" \
      --add-flags "-Djava.library.path='${librxtx_java}/lib'" \
      --add-flags "-cp ${classpath} com.dreikraft.axbo.Axbo"
  '';
}
