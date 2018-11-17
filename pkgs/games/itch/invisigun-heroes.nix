{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  saveDir = "Sombr Studio/Invisigun Heroes";
  version = "1.6.101";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "0l3dqkda9xxvlx5jbb5s3gp8yblzvp2k2wjsg8h9xx860c5nj3cy";
  };

  sandbox.paths.required = [ "$HOME/Invisigun Heroes" ];
}
