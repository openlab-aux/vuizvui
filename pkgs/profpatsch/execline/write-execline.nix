{ pkgs }:
let
  escape = import ./escape.nix { inherit (pkgs) lib; };

  # Write a list of execline argv parameters to an execline script.
  # Everything is escaped correctly.
  # TODO upstream into nixpkgs
  writeExeclineCommon = writer: name: {
     # "var": substitute readNArgs variables and start $@ from the (readNArgs+1)th argument
     # "var-full": substitute readNArgs variables and start $@ from $0
     # "env": don’t substitute, set # and 0…n environment vaariables, where n=$#
     # "none": don’t substitute or set any positional arguments
     # "env-no-push": like "env", but bypass the push-phase. Not recommended.
     argMode ? "var",
     # Number of arguments to be substituted as variables (passed to "var"/"-s" or "var-full"/"-S"
     readNArgs ? 0,
  }: argList:
   let
     env =
       if      argMode == "var" then "s${toString readNArgs}"
       else if argMode == "var-full" then "S${toString readNArgs}"
       else if argMode == "env" then ""
       else if argMode == "none" then "P"
       else if argMode == "env-no-push" then "p"
       else abort ''"${toString argMode}" is not a valid argMode, use one of "var", "var-full", "env", "none", "env-no-push".'';
   in writer name ''
    #!${pkgs.execline}/bin/execlineb -W${env}
    ${escape.escapeExecline argList}
  '';
  writeExecline = writeExeclineCommon pkgs.writeScript;
  writeExeclineBin = writeExeclineCommon pkgs.writeScriptBin;

in {
  inherit writeExecline writeExeclineBin;
}
