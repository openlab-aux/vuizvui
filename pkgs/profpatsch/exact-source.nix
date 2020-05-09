# SPDX-License-Identifier: MIT
# Created by Graham Christensen
# version from https://github.com/grahamc/mayday/blob/c48f7583e622fe2e695a2a929de34679e5818816/exact-source.nix

let
  # Require that every path specified does exist.
  #
  # By default, Nix won't complain if you refer to a missing file
  # if you don't actually use it:
  #
  #     nix-repl> ./bogus
  #     /home/grahamc/playground/bogus
  #
  #     nix-repl> toString ./bogus
  #     "/home/grahamc/playground/bogus"
  #
  # so in order for this interface to be *exact*, we must
  # specifically require every provided path exists:
  #
  #     nix-repl> "${./bogus}"
  #     error: getting attributes of path
  #     '/home/grahamc/playground/bogus': No such file or
  #     directory
  requireAllPathsExist = paths: let
    validation = builtins.map (path: "${path}") paths;
  in
    builtins.deepSeq validation paths;

  # Break down a given path in to a list of all of the path and
  # its parent directories.
  #
  # `builtins.path` / `builtins.filterSource` will ask about
  # a containing directory, and we must say YES otherwise it will
  # not include anything below it.
  #
  # Concretely, convert: "/foo/baz/tux" in to:
  #     [ "/foo/baz/tux" "/foo/baz" "/foo" ]
  recursivelyPopDir = path:
    if path == "/" then []
    else [ path ] ++ (recursivelyPopDir (builtins.dirOf path));

  # Given a list of of strings, dedup the list and return a
  # list of all unique strings.
  #
  # Note: only works on strings ;):
  #
  # First convert [ "foo" "foo" "bar" ] in to:
  #     [
  #       { name = "foo"; value = ""; }
  #       { name = "foo"; value = ""; }
  #       { name = "bar"; value = ""; }
  #     ]
  # then convert that to { "foo" = ""; "bar" = ""; }
  # then get the attribute names, "foo" and "bar".
  dedup = strings: let
    name_value_pairs = builtins.map
      (string: { name = string; value = ""; })
      strings;
    attrset_of_strings = builtins.listToAttrs name_value_pairs;
  in
    builtins.attrNames attrset_of_strings;

  exactSource = source_root: paths: let
    all_possible_paths = let
      # Convert all the paths in to relative paths on disk.
      # ie: stringPaths will contain [ "/home/grahamc/playground/..." ];
      # instead of /nix/store paths.
      string_paths = builtins.map toString
        (requireAllPathsExist paths);

      all_paths_with_duplicates = builtins.concatMap
        recursivelyPopDir
        string_paths;
    in
      dedup all_paths_with_duplicates;

    pathIsSpecified = path:
      builtins.elem path all_possible_paths;
  in
    builtins.path {
      path = source_root;
      filter = (path: _type: pathIsSpecified path);
    };
in exactSource
