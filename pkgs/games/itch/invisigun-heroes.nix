{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  version = "1.5.50";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "0gyk683sa8gkx5dl78jac905mhfv1kfjdslpczh2pm2fmd5n651x";
  };
}
