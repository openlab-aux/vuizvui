{ stdenv, fetchurl }:

let
  bfqVersion = "v7r7";
  kernelVersion = "4.0";
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
      sha256 = "0gi4qwk2wn2lr707ybh800fx4ygx5jw8glph2rqgqz2qg5pph4zz";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${kernelVersion}";
      sha256 = "1sn567adj20mzn2cbg5xppsfgipcihdwk7xd2ndnxw4hnyjhl2ll";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for-${kernelVersion}.0";
      sha256 = "1fz6rhb8sh4bi3bwb40014gaazlnx0wwv2ksxkxipsg0gnbdzdg8";
    })
  ];

  patch = stdenv.mkDerivation {
    name = "bfqsched-${version}.patch";
    inherit allPatches;
    buildCommand = ''
      cat $allPatches > "$out"
    '';
  };

in {
  name = "bfqsched-${version}";
  inherit version patch;
}
