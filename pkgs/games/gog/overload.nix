{ buildUnity, fetchGog }:

buildUnity {
  name = "overload";
  fullName = "Overload";
  saveDir = "Revival/Overload";
  version = "1.0.1839";

  src = fetchGog {
    productId = 1309632191;
    downloadName = "en3installer0";
    sha256 = "1r42ll6k2xif405rp85gn3sbhacrhf1kkpqx2ahp8j5f9alscdxm";
  };
}
