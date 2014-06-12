{ stdenv, fetchurl }:

let
  version = "3.15.0-v7r4";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r4-3.15";
      sha256 = "1l5yxplk1zxi87ckk89fh5cif0bc5k5wdaw99h23yhkq614ibajs";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r4-I-O-sched-for-3.15";
      sha256 = "1n0qjzvwhc7ng5xxki9cl4dn6pwfm2dirwjihfp9v1y9b9p9rxcp";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r4-for-3.15.0";
      sha256 = "1ag8k4wy877zpyxhiwmrd5v1p0cvql3lv8xjppqby7hbx5rba3pp";
    })
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
