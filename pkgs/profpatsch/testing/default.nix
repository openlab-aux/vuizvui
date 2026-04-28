{ runCommand, lib }:

let

  /* Realize drvDep, then return drvOut if that succeeds.
   * This can be used to make drvOut depend on the
   * build success of drvDep without making drvDep a
   * dependency of drvOut
   * => drvOut is not rebuilt if drvDep changes
   */
  drvSeq = drvDep: drvOut: drvSeqL [drvDep] drvOut;

  /* drvSeq, but take a list of drvDeps. */
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
   * Tests are shell fragments which test the
   * derivation and should result in failure if
   * the functionality is not as expected. */
  withTests = tests: drv:
    assert lib.isDerivation drv; # drv needs to be a derivation!
    let testDrvs = lib.mapAttrsToList
      (name: testScript:
        runCommand "${drv.name}-test-${name}" {
          preferLocalBuild = true;
          allowSubstitutes = false;
        } ''
          ${testScript}
          touch "$out"
      '') tests;
    in drvSeqL testDrvs drv;


in
  { inherit drvSeq drvSeqL withTests; }
