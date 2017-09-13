{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  version = "1.5.30";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "07iskccdmygnx70naaa3fcac1ayrhmq82cypddsnihc3gkw7rwrd";
  };
}
