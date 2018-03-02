{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.25";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "0kd6z70m85p0iyhipcgxw8x43h662jiwk4r3s1q94nn9zpndwj5s";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
