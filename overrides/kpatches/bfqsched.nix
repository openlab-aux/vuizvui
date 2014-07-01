{ stdenv, fetchurl }:

let
  bfqVersion = "v7r5";
  kernelVersion = "3.15";
  version = "${kernelVersion}.0-${bfqVersion}";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-"
           + "${bfqVersion}-${kernelVersion}";
      sha256 = "16vrdg9ky7q89sb7gp8as7qslqg1zy1n28cnyshm91qnf49wfh7g";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${kernelVersion}";
      sha256 = "1gf4y5i9prhg9ls2g8apaiygm538khhpp35dahgv59hr3sfwsghs";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for-${kernelVersion}.0";
      sha256 = "1q3fn6gl3lhvbqfhaarpv399id9sa32zx6ygqx4x98zmixwrsn9z";
    })
    ./bfqsched.patch
  ];

  patch = stdenv.mkDerivation {
    name = "bfqsched-${version}.patch";
    inherit allPatches;
    buildCommand = ''
      sed -e 's/IS_SUBSYS_ENABLED/IS_ENABLED/' \
          -e '/SUBSYS/s/perf/&_event/' $allPatches > "$out"
    '';
  };

in {
  name = "bfqsched-${version}";
  inherit version patch;
}
