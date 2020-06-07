{ buildUnity, fetchGog }:

buildUnity {
  name = "gibbous";
  fullName = "Gibbous - A Cthulhu Adventure";
  saveDir = "StuckInAttic/Gibbous - A Cthulhu Adventure";
  version = "1.8";

  src = fetchGog {
    productId = 1376056537;
    downloadName = "en3installer0";
    sha256 = "0d0r2dy5sk54msmsmg4hh5ldlxw18vz0zvsn8c885q0gd3pmygk0";
  };
}
