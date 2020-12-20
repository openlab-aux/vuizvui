{ buildUnity, fetchGog }:

buildUnity {
  name = "graveyard-keeper";
  fullName = "Graveyard Keeper";
  saveDir = "Lazy Bear Games/Graveyard Keeper";
  version = "1.310";

  src = fetchGog {
    productId = 1780408621;
    sha256 = "0a9a53dm46q66g9zz5w1937a4635vnihj8xpbi6lxhfnlpf0cxl3";
  };
}
