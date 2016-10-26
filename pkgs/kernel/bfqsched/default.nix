{ stdenv, fetchurl }:

let
  bfqVersion = "v8r2";
  kernelVersion = "4.7.0";
  version = "${kernelVersion}-${bfqVersion}";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r11-4.7.0";
      sha256 = "116jfdgjgmy1fv6kzz7dm1c7rjy1pbsfzzcjv5mgsb7pnaxq3gd6";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r11-I-O-sched-for-4.7.0";
      sha256 = "0wjmnym2ycglx42f513n97b45x3xqi33q7z4cs0aiz0zbblm8jql";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r11-for";
      sha256 = "0898aklynxb9dr0nb0kdhc2incjkjihds9dakxvdy0mwjqr0jd6v";
    })
    (fetchPatch {
      name = "0004-block-bfq-turn-BFQ-v7r11-for-4.7.0-into-BFQ-v8r2-for";
      sha256 = "1aljji6ww73h0vpimbcmk0hj886m6fnnbahgmrkfcq1gc0n0484n";
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
