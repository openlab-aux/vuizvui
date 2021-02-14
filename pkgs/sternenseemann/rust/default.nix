{ writeRustSimpleLib
, testRustSimple
}:

let

  temp = testRustSimple
    (writeRustSimpleLib "temp" {
      release = false;
      verbose = true;
      meta = {
        description = "Tiny temp dir/file crate for rust";
      };
    } ./temp.rs);

in {
  inherit
    temp
    ;
  }
