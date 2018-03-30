{ buildUnity, fetchHumbleBundle }:

buildUnity rec {
  name = "minimetro";
  version = "39";
  fullName = "Mini Metro";
  saveDir = "Dinosaur Polo Club/Mini Metro";

  src = fetchHumbleBundle {
    name = "MiniMetro-release-39-linux.tar.gz";
    machineName = "minimetro_linux";
    downloadName = "Download";
    md5 = "3e7afefbcc68b6295821394e31f5e48b";
  };
}
