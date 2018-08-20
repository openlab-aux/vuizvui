{ buildUnity, fetchGog }:

buildUnity {
  name = "overload";
  fullName = "Overload";
  saveDir = "Revival/Overload";
  version = "1.0.1854";

  src = fetchGog {
    productId = 1309632191;
    downloadName = "en3installer0";
    sha256 = "1qyd78xzd39763dmrb5rb8g0v0qi45jjkb9id9gjvvmh41m0731i";
  };
}
