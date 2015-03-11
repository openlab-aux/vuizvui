{ stdenv, fetchurl }:

let
  bfqVersion = "v7r7";
  kernelVersion = "3.19";
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
      sha256 = "1qlk1hcirf3x9h80f9crnyqgxvw79xi89rwv0sh4hsd1lw3sbi7s";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${kernelVersion}";
      sha256 = "04fbynd6k7g92rbyciswx3mswxr0amvp75wzdpn57wwjv5vlh7b2";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for-${kernelVersion}.0";
      sha256 = "17ypb5y7wkr719vy00wm09szmqnlpzpvkcq5phivdxd27417828m";
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
