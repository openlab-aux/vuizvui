{ pkgs, runExeclineLocal, getBins, drvSeqL }:
let
  bins = getBins pkgs.coreutils [ "ln" "ls" "touch" ];

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

  /* Takes a `buildRustCrate` derivation as an input,
    * builds it with `{ buildTests = true; }` and runs
    * all tests found in its `tests` dir. If they are
    * all successful, `$out` will point to the crate
    * built with `{ buildTests = false; }`, otherwise
    * it will fail to build.
    *
    * See also the documentation on `drvSeqL` which is
    * used to implement this behavior.
    */
  testRustSimple = rustDrv:
    let
      crate = buildTests: rustDrv.override { inherit buildTests; };
      tests = runExeclineLocal "${rustDrv.name}-tests-run" {} [
        "importas" "out" "out"
        "if" [
          "pipeline" [ bins.ls "${crate true}/tests" ]
          "forstdin" "-o0" "test"
          "importas" "test" "test"
          "${crate true}/tests/$test"
        ]
        bins.touch "$out"
      ];
    in drvSeqL [ tests ] (crate false);

in {
  inherit
    writeRustSimple
    writeRustSimpleBin
    writeRustSimpleLib
    testRustSimple
    ;
}
