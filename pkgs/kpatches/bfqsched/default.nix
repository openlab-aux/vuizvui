{ stdenv, fetchurl }:

let
  bfqVersion = "v7r9";
  kernelVersion = "4.2";
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
      sha256 = "1998g05gj2vcwxhccjsa4nch3pyzm2qlc1k9q688f70j19qbnybk";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${kernelVersion}";
      sha256 = "1p5xxpzbmch03r7d0wpfr4x1vhy1h7mv2lwck7lq3184mcscqgkl";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for-${kernelVersion}.0";
      sha256 = "10bzgdz4n2q6inv1w8v0qdhl0pl1gndn1iz97qkxdi0zfabc574d";
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
