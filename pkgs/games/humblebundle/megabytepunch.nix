{ buildUnity, fetchHumbleBundle }:

buildUnity {
  name = "megabytepunch";
  fullName = "MegabytePunch";
  saveDir = "Reptile/Megabyte Punch";
  version = "1.12";

  src = fetchHumbleBundle {
    machineName = "megabytepunch_linux";
    suffix = "tar.gz";
    md5 = "13487ae35c99817ce5f19b45fa51158b";
  };
}
