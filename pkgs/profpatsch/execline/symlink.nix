{ lib, s6-portable-utils, coreutils, runExecline, toNetstring }:
# DrvPath :: path relative to the derivation
# AbsPath :: absolute path in the store
#    Name
# -> List (Symlink { dest: DrvPath, orig: AbsPath })
# -> Drv
name: links:

runExecline name {
  derivationArgs = {
    pathTuples = lib.concatMapStrings
      ({dest, orig}: toNetstring
        (toNetstring dest + (toNetstring orig)))
      links;
    passAsFile = [ "pathTuples" ];
    PATH = lib.makeBinPath [ s6-portable-utils ];
  };
} [
  "importas" "-ui" "p" "pathTuplesPath"
  "importas" "-ui" "out" "out"
  "forbacktickx" "-d" "" "destorig" [ "${coreutils}/bin/cat" "$p" ]
    "importas" "-ui" "do" "destorig"
    "multidefine" "-d" "" "$do" [ "destsuffix" "orig" ]
    "define" "dest" ''''${out}/''${destsuffix}''

    # this call happens for every file, not very efficient
    "foreground" [
      "backtick" "-n" "d" [ "s6-dirname" "$dest" ]
      "importas" "-ui" "d" "d"
      "s6-mkdir" "-p" "$d"
    ]

    "ifthenelse" [ "eltest" "-L" "$orig" ] [
      "backtick" "-n" "res" [ "s6-linkname" "-f" "$orig" ]
      "importas" "-ui" "res" "res"
      "s6-ln" "-fs" "$res" "$dest"
    ] [
      "s6-ln" "-fs" "$orig" "$dest"
    ]
]
