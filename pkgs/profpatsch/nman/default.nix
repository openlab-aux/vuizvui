{ lib, writeRustSimpleBin }:

writeRustSimpleBin "nman" {
  meta = {
    license = lib.licenses.gpl3Only;
    description = "Open man page in a temporary nix-shell";
  };
} ./nman.rs
