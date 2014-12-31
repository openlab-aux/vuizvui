{ stdenv, fetchHumbleBundle }:

stdenv.mkDerivation rec {
  name = "rocketbirds-${version}";
  version = "20130917";

  src = fetchHumbleBundle {
    name = "Rocketbirds${version}.sh";
    md5 = "7c5e6da4cd7fc7f2f51861f8b96a386f";
  };
}
