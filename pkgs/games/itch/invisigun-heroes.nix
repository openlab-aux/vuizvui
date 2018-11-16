{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.100";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "14drg7phbgd7l8zrmcw5njrvs0alilayrdd8xinljzz0i3wmdnhg";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
