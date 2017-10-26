{ buildUnity, fetchItch }:

buildUnity rec {
  name = "invisigun-heroes";
  fullName = "Invisigun Heroes";
  version = "1.5.70";

  src = fetchItch {
    name = "${name}-${version}.zip";
    gameId = 25561;
    uploadId = 208583;
    version = "v${version}";
    sha256 = "149g3bsgyqal3m1h2rh5h0m3dc39ky5n81y8plx3xg9r2b1sfhsn";
  };
}
