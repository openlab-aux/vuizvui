{ buildUnity, fetchGog }:

buildUnity {
  name = "knights-and-bikes";
  fullName = "Knights and Bikes";
  saveDir = "Foam Sword/Knights and Bikes";
  version = "1.07";

  src = fetchGog {
    productId = 1687611835;
    sha256 = "0s527s08mkc54ynk18shl1fnr8pjcnfd7qjdilgi9xhghd5sj6g1";
  };
}
