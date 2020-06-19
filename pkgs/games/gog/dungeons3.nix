{ buildUnity, fetchGog, mono }:

buildUnity {
  name = "dungeons3";
  fullName = "Dungeons3";
  saveDir = "Realmforge Studios GmbH/Dungeons 3";
  version = "1.4.4";

  src = fetchGog {
    productId = 1346232158;
    sha256 = "1m4dvb91nfwxbgb76n8saznaaif053vb5wkdllb7imdbqqwlfsmy";
  };

  buildInputs = [ mono ];
}
