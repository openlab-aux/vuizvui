{ stdenv, lib, pkgs, getBins, testing }:

# todo: factor out calling tests
let
  it = import ./run-execline.nix {
    bin = getBins pkgs.execline [ "execlineb" "redirfd" "importas" "exec" ];
    inherit stdenv lib;
  };
  itLocal = name: args: execline:
    it name (args // {
      derivationArgs = args.derivationArgs or {} // {
        preferLocalBuild = true;
        allowSubstitutes = false;
      };
    }) execline;

  tests = import ./run-execline-tests.nix {
    # can't use runExeclineLocal in the tests,
    # because it is tested by the tests (well, it does
    # work, but then you have to run the tests every time)
    runExecline = it;
    inherit (testing) drvSeqL;
    inherit (pkgs) coreutils;
    inherit stdenv;
    bin = (getBins pkgs.execline [
             "execlineb"
             "eltest"
             { use = "if"; as = "execlineIf"; }
             "redirfd" "importas"
           ])
       // (getBins pkgs.s6-portable-utils
            [ "s6-cat" "s6-grep" "s6-touch" "s6-chmod" ]);
   };
in {
  runExecline = it;
  runExeclineLocal = name: args: execline:
    testing.drvSeqL tests (itLocal name args execline);

  # This is just a stupid workaround to prevent nix restricted mode
  # from stumbling over the symlink in the derivation output.
  runExeclineLocalNoSeqL = name: args: execline:
    itLocal name args execline;
}
