{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.15";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "0a8lf5w7xb3pzakf4b3adnk1bi7mzqmxvjamci6a6snazhyghrgp";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
