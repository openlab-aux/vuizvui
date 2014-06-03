{ stdenv, fetchurl }:

let
  version = "3.14.0-v7r4";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r4-3.14";
      sha256 = "06flwmri6svjamjliv9zh23vfa3acydix23bljs294zhfd9gg3mk";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r4-I-O-sched-for-3.14";
      sha256 = "1mjcr45hr1i6s8sdhd0g83wy638jbvv6z33g93xiyndw4idkggs4";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r4-for-3.14.0";
      sha256 = "1857x7m9m33yvrzd19fzj49zjll93adjis9xw6rnmvxr5m6m3wq9";
    })
    ./bfqsched-3.15.patch
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
