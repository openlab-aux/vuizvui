{ buildUnity, fetchGog }:

buildUnity {
  name = "kingdoms-and-castles";
  fullName = "KingdomsAndCastles";
  saveDir = "LionShield/Kingdoms and Castles";
  version = "115r12";

  src = fetchGog {
    productId = 2067763543;
    sha256 = "1ag03piq09z7hljcbs145hyj8z0gjcvffj99znf3mnbw2qipb7pq";
  };
}
