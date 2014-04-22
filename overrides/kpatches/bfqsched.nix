{ stdenv, fetchurl }:

let
  version = "3.14.0-v7r2";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r2-3.14";
      sha256 = "04vjygsg4hp81i0f78jkv51s4m7w6k3qxhrpjrasrkgyi3wxh15l";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r2-I-O-sched-for-3.14";
      sha256 = "1bz6rzg2fakfhh0grpr7z0nl85bkibl6lcw4wgmh34f3kvq89gxs";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r2-for-3.14.0";
      sha256 = "0dsj6hvk0dh1f3p88zyqm31sj0hczjm8k9bmsqvhn38z0c88qi8v";
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
