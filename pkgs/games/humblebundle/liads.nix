{ buildUnity, fetchHumbleBundle }:

buildUnity {
  name = "liads";
  fullName = "LoversInADangerousSpacetime";
  saveDir = "AsteroidBase/LoversInADangerousSpacetime";
  version = "20180427";

  src = fetchHumbleBundle {
    machineName = "loversinadangerousspacetime_linux";
    suffix = "zip";
    md5 = "67b6bc5ba5590fb50e95996b267f8c60";
  };
}
