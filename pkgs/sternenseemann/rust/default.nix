{ writeRustSimpleLib
, buildRustCrate
}:

let

  temp-common = tests: writeRustSimpleLib "temp" {
    buildTests = tests;
    release = false;
    verbose = true;
    meta = {
      description = "Tiny temp dir/file crate for rust";
    };
  } ./temp.rs;

  temp = temp-common false;
  temp-tests = temp-common true;

in {
  inherit
    temp
    temp-tests
    ;
  }
