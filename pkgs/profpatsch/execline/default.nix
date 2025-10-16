{ pkgs, writeRustSimpleLib, profpatsch, ... }:

let
  el-semicolon = writeRustSimpleLib "el_semicolon" {
    release = false;
    verbose = true;
  } ./el_semicolon.rs;

  el-exec = writeRustSimpleLib "el_exec" {
    dependencies = [ profpatsch.rust-deps.libc ];
    buildInputs = [ pkgs.skalibs ];
    release = false;
    verbose = true;
  } ./el_exec.rs;

  el-substitute = writeRustSimpleLib "el_substitute" {
    dependencies = [ profpatsch.rust-deps.libc profpatsch.rust-deps.errno ];
    buildInputs = [ pkgs.skalibs pkgs.execline ];
    release = false;
    verbose = true;
  } ./el_substitute.rs;

  runblock = (import ./runblock.nix { inherit pkgs; }).runblock;

in {
  inherit
    el-semicolon
    el-exec
    el-substitute
    runblock
    ;
}
