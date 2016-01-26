{ stdenv, fetchurl }:

let
  bfqVersion = "v7r11";
  kernelVersion = "4.4";
  fullKernelVersion = "${kernelVersion}.0";
  version = "${fullKernelVersion}-${bfqVersion}";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-"
           + "${bfqVersion}-${fullKernelVersion}";
      sha256 = "1kmlfz63610zc4lxhanjsn4hhw43cdsbk3pyaij723vbd7619kyi";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-"
           + "${bfqVersion}-I-O-sched-for-${fullKernelVersion}";
      sha256 = "1i5jqkxglp3ah76i4vyi13pnmjkr6qlqy69qbaj2132vijqkyz5i";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-"
           + "${bfqVersion}-for";
      sha256 = "09bv31s8d2aphi3d9py4sz1gcvyb5645a8s7zj614a56hv11p8k9";
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
