{ buildUnity, fetchGog, libudev }:

buildUnity {
  name = "sunless-skies";
  fullName = "Sunless Skies";
  saveDir = "Failbetter Games/Sunless Skies";
  version = "1.3.6";

  src = fetchGog {
    productId = 1771268779;
    sha256 = "1j6mdjy4x6xra2bx1f6iz7ywhg9cz340kfvlihcnmll66jxqrrk7";
  };
}
