{ stdenv, runCommand, lib
, runExecline, s6TouchCommand, s6EchoCommand }:

let

  /* Realize drvDep, then return drvOut if that succeds.
   * This can be used to make drvOut depend on the
   * build success of drvDep without making drvDep a
   * dependency of drvOut
   * => drvOut is not rebuilt if drvDep changes
   */
  drvSeq = drvDep: drvOut: drvSeqL [drvDep] drvOut;

  /* TODO DOCS */
  drvSeqL = drvDeps: drvOut: let
    drvOutOutputs = drvOut.outputs or ["out"];
  in
    runCommand drvOut.name {
      # we inherit all attributes in order to replicate
      # the original derivation as much as possible
      outputs = drvOutOutputs;
      passthru = drvOut.drvAttrs;
      preferLocalBuild = true;
      allowSubstitutes = false;
      # depend on drvDeps (by putting it in builder context)
      inherit drvDeps;
    }
    # the outputs of the original derivation are replicated
    # by creating a symlink to the old output path
    (lib.concatMapStrings (output: ''
      target=${lib.escapeShellArg drvOut.${output}}
      # if the target is already a symlink, follow it until it’s not;
      # this is done to prevent too many dereferences
      target=$(readlink -e "$target")
      # link to the output
      ln -s "$target" "${"$"}${output}"
    '') drvOutOutputs);

  /* Takes a derivation and an attribute set of
   * test names to tests.
   * Tests can be constructed by calling test
   * functions like `bashTest` or `execlineTest`.
   * They generally take scripts that
   * is not sucessful and succeed otherwise.
   */
  withTests = tests: drv:
    assert lib.isDerivation drv; # drv needs to be a derivation!
    let testDrvs = lib.mapAttrsToList
          (testName: testFun: testFun {
            drvName = "${drv.name}-test-${testName}";
          }) tests;
    in drvSeqL testDrvs drv;

  # /* Constructs a test from a bash script.
  #  * The test will fail if the bash script exits
  #  * with an exit code other than 0. */
  # bashTest = testScript: { drvName }:
  #   runCommand drvName {
  #     preferLocalBuild = true;
  #     allowSubstitutes = false;
  #   } ''
  #     ${testScript}
  #     touch "$out"
  #   '';

  # /* Constructs a test from an execline script.
  #  * The test will fail if the bash script exits
  #  * with an exit code other than 0. */
  # execlineTest = testScript: { drvName }:
  #   runExecline {
  #     name = drvName;
  #     execline = testScript;
  #     builderWrapper = runExecline {
  #       name = "touch-out";
  #       execline = ''
  #         importas -i out out
  #         ifte
  #           # if $@ succeeds, $out is touched
  #           { ${s6TouchCommand} $out }
  #           # otherwise we return the exit code
  #           { importas exit ?
  #             ${s6EchoCommand} $exit }
  #           # condition
  #           $@
  #       '';
  #     derivationArgs = {
  #       preferLocalBuild = true;
  #       allowSubstitutes = false;
  #     };
  #   };
  # };

in
  { inherit drvSeq drvSeqL withTests; }
