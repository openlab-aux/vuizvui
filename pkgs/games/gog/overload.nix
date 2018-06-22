{ buildUnity, fetchGog, unzip }:

buildUnity {
  name = "overload";
  fullName = "Overload";
  saveDir = "Revival/Overload";
  version = "1.0.1829";

  src = fetchGog {
    productId = 1309632191;
    downloadName = "en3installer0";
    sha256 = "0gw2d78smixzkywhwi1cskhr0mwpn4im5r4k1n0pgavb30payhss";
  };

  unpackCmd = "${unzip}/bin/unzip -qq \"$curSrc\" 'data/noarch/game/*' || :";
}
