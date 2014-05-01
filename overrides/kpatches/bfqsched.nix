{ stdenv, fetchurl }:

let
  version = "3.14.0-v7r3";

  baseURL = "http://algo.ing.unimo.it/people/paolo/disk_sched/patches";

  fetchPatch = { name, sha256 }: fetchurl {
    url = "${baseURL}/${version}/${name}.patch";
    inherit sha256;
  };

  allPatches = [
    (fetchPatch {
      name = "0001-block-cgroups-kconfig-build-bits-for-BFQ-v7r3-3.14";
      sha256 = "1md84ld7lsvxwmzyd9r5znd66vgdljxvqwj47fzy2idsqyn6gjwi";
    })
    (fetchPatch {
      name = "0002-block-introduce-the-BFQ-v7r3-I-O-sched-for-3.14";
      sha256 = "1vavp37gwwlr1zxha2wl9yc1rhllcdb4yyahxkr1iwajwrfm3xwi";
    })
    (fetchPatch {
      name = "0003-block-bfq-add-Early-Queue-Merge-EQM-to-BFQ-v7r3-for-3.14.0";
      sha256 = "1bpparpn3rglrdcwgs6hn8ggwhngrx1cy9rqn2mg9jwdz22lrlhv";
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
