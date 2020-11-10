{ buildUnity, fetchGog }:

buildUnity {
  name = "alwas-legacy";
  fullName = "AlwasLegacy";
  saveDir = "EldenPixels/Alwa's Legacy";
  version = "1.6.6.56";

  src = fetchGog {
    productId = 2118716711;
    sha256 = "0prk8dyxb4jvwjddvm9h3vvgpb4phhr6y5rdlk5az387j7a4ffj4";
  };
}
