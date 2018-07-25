{ lib, coreutils, s6-portable-utils, symlink }:
let
  example = {from, as, just, ...}:
    [
      (from coreutils [
        (just "echo")
        (as "core-ls" "ls")
      ])
      (from s6-portable-utils [
        (as "ls" "s6-ls")
        (just "s6-echo")
      ])
    ];

  runImport = impsFn:
    let
      combinators = rec {
        from = source: imports: {
          inherit source imports;
        };
        as = newname: oldname: {
          inherit oldname newname;
        };
        just = x: as x x;
      };

      # Drv -> As -> Symlink
      toBin = module: {oldname, newname}: {
        dest = "bin/${newname}";
        orig = "${module}/bin/${oldname}";
      };
      # List (Import { source: Drv
      #              , imports: List (As { oldname: String
      #                                  , newname: String }))
      # -> Drv
      run = imps:
        symlink "foo" (lib.concatLists
          (map ({source, imports}:
                   map (toBin source) imports)
               imps));

    # TODO: typecheck w/ newtypes
    in run (impsFn combinators);

in runImport example
