{ buildUnity, fetchGog, unzip }:

buildUnity {
  name = "party-hard";
  fullName = "PartyHardGame";
  saveDir = "PinoklGames/PartyHardGame";
  version = "1.4.030r";

  src = fetchGog {
    productId = 1236689966;
    downloadName = "en3installer2";
    sha256 = "00sb76w0v4b2izfwx1kr53frrgg8rg5d0qgpj8z3xq8frnv1fmi4";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/noarch/game/*' || :";
}
