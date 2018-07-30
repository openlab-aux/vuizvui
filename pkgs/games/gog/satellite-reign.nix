{ buildUnity, fetchGog }:

buildUnity {
  name = "satellite-reign";
  fullName = "SatelliteReignLinux";
  saveDir = "5 Lives Studios/SatelliteReign";
  version = "1.13.06";

  src = fetchGog {
    productId = 1428054996;
    downloadName = "en3installer9";
    sha256 = "0wpkpqrcli2772g6l9yab38vbjh1by4cbpa397fqvhny247qdz5k";
  };

  sandbox.paths.required = [ "$XDG_DATA_HOME/SatelliteReign" ];
}
