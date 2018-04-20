{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.50";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "1p5zx8xs0gjsimkwli23cw41r55vy7dy9rlrqiwk9lvm9si4rn9i";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
