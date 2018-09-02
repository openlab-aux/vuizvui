{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.81";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "0g3z5yy1c2s2vszs31sc7vlzw2x3pvsw9vnzcb3k9593623iw2hj";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
