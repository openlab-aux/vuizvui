{ stdenv, importasCommand, execlinebCommand }:
{ name
# the execline script
, execline
# additional arguments to pass to the derivation
, derivationArgs ? {}
}:

# those arguments can’t be overwritten
assert !derivationArgs ? system;
assert !derivationArgs ? name;
assert !derivationArgs ? builder;
assert !derivationArgs ? args;

derivation (derivationArgs // {
  inherit (stdenv) system;
  inherit name;

  # okay, `builtins.toFile` does not accept strings
  # that reference drv outputs. This means we need
  # to pass the script as envvar;
  # this might clash with another passed envar,
  # so we give it a long & unique name
  _runExeclineScript = execline;
  passAsFile = [ "_runExeclineScript" ]
            ++ derivationArgs.passAsFile or [];

  builder = importasCommand;
  args = [
    "-ui"                    # drop the envvar afterwards
    "script"                 # substitution name
    "_runExeclineScriptPath" # passed script file
    execlinebCommand         # the actual invocation
    "-P"                     # ignore command line arguments
    "-W"                     # die on syntax error
    "$script"                # substituted by importas
  ];
})
