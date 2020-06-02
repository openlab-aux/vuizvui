{ pkgs, runExeclineLocal, getBins }:
let
  bins = getBins pkgs.coreutils [ "ln" ];

  writeRustSimple = name: args: srcFile:
    linkTo name "${writeRustSimpleBin name args srcFile}/bin/${name}";

  linkTo = name: path: runExeclineLocal name {} [
    "importas" "out" "out"
    bins.ln "-sT" path "$out"
  ];

  writeRustSimpleBin = name: { dependencies ? [], ... }@args: srcFile: pkgs.buildRustCrate ({
      pname = name;
      version = "1.0.0";
      crateName = name;
      crateBin = [ name ];
      dependencies = dependencies;
      src = pkgs.runCommandLocal "write-main.rs" {} ''
        mkdir -p $out/src/bin
        cp ${srcFile} $out/src/bin/${name}.rs
        find $out
      '';
    } // args);

  writeRustSimpleLib = name: { dependencies ? [], ... }@args: srcFile: pkgs.buildRustCrate ({
      pname = name;
      version = "1.0.0";
      crateName = name;
      dependencies = dependencies;
      src = pkgs.runCommandLocal "write-lib.rs" {} ''
        mkdir -p $out/src
        cp ${srcFile} $out/src/lib.rs
        find $out
      '';
    } // args);

in {
  inherit
    writeRustSimple
    writeRustSimpleBin
    writeRustSimpleLib
    ;
}
