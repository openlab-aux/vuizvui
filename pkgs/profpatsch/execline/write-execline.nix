{ pkgs }:
let
  escape = import ./escape.nix { inherit (pkgs) lib; };

  # Write a list of execline argv parameters to an execline script.
  # Everything is escaped correctly.
  # TODO upstream into nixpkgs
  writeExeclineCommon = writer: name: { readNArgs ? 0 }: argList: writer name ''
    #!${pkgs.execline}/bin/execlineb -Ws${toString readNArgs}
    ${escape.escapeExecline argList}
  '';
  writeExecline = writeExeclineCommon pkgs.writeScript;
  writeExeclineBin = writeExeclineCommon pkgs.writeScriptBin;

in {
  inherit writeExecline writeExeclineBin;
}
