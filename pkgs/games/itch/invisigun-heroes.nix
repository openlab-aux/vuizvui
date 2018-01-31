{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.1";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "1jvdqrr7rysvgmyn3zyjyydwl8hwrdisis4wznpjhl02nplmhnqx";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
