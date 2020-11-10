{ buildUnity, fetchGog, lib }:

buildUnity {
  name = "the-longing";
  fullName = "The Longing";
  saveDir = "Studio Seufz/The Longing";
  version = "1.0.7";

  src = fetchGog {
    productId = 1091099749;
    sha256 = "18gzzgl7dkgbv2a6ibj3bxc409asnad6b2yaxg8mp8ya7wyw8wv0";
  };
}
