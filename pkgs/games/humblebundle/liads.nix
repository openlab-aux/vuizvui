{ buildUnity, fetchHumbleBundle }:

buildUnity {
  name = "liads";
  version = "20180427";

  fullName = "LoversInADangerousSpacetime";
  saveDir = "AsteroidBase/LoversInADangerousSpacetime";
  sandbox.paths.required = [ "$XDG_DATA_HOME/LoversInADangerousSpacetime" ];

  src = fetchHumbleBundle {
    machineName = "loversinadangerousspacetime_linux";
    suffix = "zip";
    md5 = "67b6bc5ba5590fb50e95996b267f8c60";
  };
}
