{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.71";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "16bcyypigk0nm7ckdfxc9hkwv8dznqs936qmkzbjb22va2j4ip0s";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
