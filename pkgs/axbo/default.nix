{ stdenv, fetchurl, fetchFromGitHub, jdk, jre, ant, makeWrapper
, commonsLogging, librxtx_java
}:

stdenv.mkDerivation rec {
  name = "axbo-research-${version}";
  version = "3.0.12";

  src = fetchFromGitHub {
    owner = "jansolo";
    repo = "aXbo-research";
    rev = "aXbo-research_${version}";
    sha256 = "0p2my5bczmwnrs3c0l9wyq3gsc5vydw0nh9n8jkdp9vm77z0kgvd";
  };

  sourceRoot = "${src.name}/aXbo-research";

  buildInputs = [ jdk ant makeWrapper ];

  buildPhase = ''
    ant -Dplatforms.JDK_1.7.home="$JAVA_HOME" jar
  '';

  extraJars = [
    "commons-beanutils-1.8.3"
    "commons-digester3-3.2"
    "jcommon-1.0.20"
    "jfreechart-1.0.16"
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
