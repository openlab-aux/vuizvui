{ stdenv, fetchurl }:

let
  bfqVersion = "v7r8";
  kernelVersion = "4.3";
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
      sha256 = "14549awmvsqwzb7912k83dmlznx5lak8gcivqjd79clrd4h65szb";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${kernelVersion}";
      sha256 = "1sw65hxjimg9w04f7ccrxhl1c8b1ddd21ni9af77cyrkp11cpdwi";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for-${kernelVersion}.0";
      sha256 = "178spkcwaz79rz8fb7h209grbgr2ca4lbgfmns4d43b7aiqhqhvp";
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
