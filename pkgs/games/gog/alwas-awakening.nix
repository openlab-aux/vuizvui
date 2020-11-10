{ buildUnity, fetchGog }:

buildUnity {
  name = "alwas-awakening";
  fullName = "AlwasAwakening";
  saveDir = "EldenPixels/Alwa's Awakening";
  version = "1.6.6.56";

  src = fetchGog {
    productId = 1396087560;
    sha256 = "0x5p3qy3cb21368r09dlpv70c93ld7810jlrqz2m1qw41rsmwd9h";
  };
}
