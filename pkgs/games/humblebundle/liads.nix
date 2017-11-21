{ buildUnity, fetchHumbleBundle }:

buildUnity {
  name = "liads";
  fullName = "LoversInADangerousSpacetime";
  saveDir = "AsteroidBase/LoversInADangerousSpacetime";
  version = "20160121";

  src = fetchHumbleBundle {
    machineName = "loversinadangerousspacetime_linux";
    suffix = "zip";
    md5 = "38927a73e1fe84620ebc876f8f039adb";
  };
}
