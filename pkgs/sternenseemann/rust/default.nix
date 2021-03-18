{ writeRustSimpleLib
, writeRustSimpleBin
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

  nix-env-diff = writeRustSimpleBin "nix-env-diff" {
    meta = {
      description = "Print changed attrs / outpath for nix-env outputs";
    };
  } ./nix-env-diff.rs;

in {
  inherit
    temp
    nix-env-diff
    ;
  }
