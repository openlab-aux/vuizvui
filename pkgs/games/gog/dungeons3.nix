{ buildUnity, fetchGog, unzip, mono }:

buildUnity {
  name = "dungeons3";
  fullName = "Dungeons3";
  saveDir = "Realmforge Studios GmbH/Dungeons 3";
  version = "1.4.4";

  src = fetchGog {
    productId = 1346232158;
    downloadName = "en3installer0";
    sha256 = "1m4dvb91nfwxbgb76n8saznaaif053vb5wkdllb7imdbqqwlfsmy";
  };

  buildInputs = [ mono ];

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/noarch/game/*' || :";
}
