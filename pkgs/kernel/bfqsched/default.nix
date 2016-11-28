{ stdenv, fetchurl }:

let
  bfqVersion = "v8r4";
  kernelVersion = "4.8.0";
  version = "${kernelVersion}-${bfqVersion}";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r11-4.8.0";
      sha256 = "0br6j334apx99kan71c6z5fnxhnv60j4rzpgzzi0jzlbn5lxkaqx";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r11-I-O-sched-to-be-ported";
      sha256 = "1ll6pqj34bhf2mqhdcr12sw45j6wc00lf483r780yy6mjdw7mlf8";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r11-to-";
      sha256 = "1vqx3sggx1bjxi7fyj4fcfn89b0d6bqvz1ba9vggl37jqaqsazp4";
    })
    (fetchPatch {
      name = "0004-Turn-BFQ-v7r11-into-BFQ-v8r4-for-4.8.0";
      sha256 = "0nazr2mqqgaz2060c4wdj6xfcxp1mgp1iyz1qq68ydzg98q6xjf3";
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
