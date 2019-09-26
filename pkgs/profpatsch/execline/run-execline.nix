{ stdenv, bin, lib }:
{ name
# the execline script as a nested list of string,
# representing the blocks;
# see docs of `escapeExecline`.
, execline
# a string to pass as stdin to the execline script
, stdin ? ""
# a program wrapping the acutal execline invocation;
# should be in Bernstein-chaining style
, builderWrapper ? bin.exec
# additional arguments to pass to the derivation
, derivationArgs ? {}
}:

# those arguments can’t be overwritten
assert !derivationArgs ? system;
assert !derivationArgs ? name;
assert !derivationArgs ? builder;
assert !derivationArgs ? args;

derivation (derivationArgs // {
  # TODO: what about cross?
  inherit (stdenv) system;
  inherit name;

  # okay, `builtins.toFile` does not accept strings
  # that reference drv outputs. This means we need
  # to pass the script and stdin as envvar;
  # this might clash with another passed envar,
  # so we give it a long & unique name
  _runExeclineScript =
    let
      escape = (import ./escape.nix { inherit lib; });
    in escape.escapeExecline execline;
  _runExeclineStdin = stdin;
  passAsFile = [
    "_runExeclineScript"
    "_runExeclineStdin"
  ] ++ derivationArgs.passAsFile or [];

  # the default, exec acts as identity executable
  builder = builderWrapper;

  args = [
    bin.importas             # import script file as $script
    "-ui"                    # drop the envvar afterwards
    "script"                 # substitution name
    "_runExeclineScriptPath" # passed script file

    # TODO: can we scrap stdin via builderWrapper?
    bin.importas             # do the same for $stdin
    "-ui"
    "stdin"
    "_runExeclineStdinPath"

    bin.redirfd              # now we
    "-r"                     # read the file
    "0"                      # into the stdin of execlineb
    "$stdin"                 # that was given via stdin

    bin.execlineb            # the actual invocation
    # TODO: depending on the use-case, -S0 might not be enough
    # in all use-cases, then a wrapper for execlineb arguments
    # should be added (-P, -S, -s).
    "-S0"                    # set $@ inside the execline script
    "-W"                     # die on syntax error
    "$script"                # substituted by importas
  ];
})
