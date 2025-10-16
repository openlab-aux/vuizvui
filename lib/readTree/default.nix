# Copyright (c) 2019 Vincent Ambo
# Copyright (c) 2020-2021 The TVL Authors
# SPDX-License-Identifier: MIT
#
# Provides a function to automatically read a filesystem structure
# into a Nix attribute set.
#
# Called with an attribute set taking the following arguments:
#
#   path: Path to a directory from which to start reading the tree.
#
#   args: Argument set to pass to each imported file.
#
#   filter: Function to filter `args` based on the tree location. This should
#           be a function of the form `args -> location -> args`, where the
#           location is a list of strings representing the path components of
#           the current readTree target. Optional.
{ ... }:

let
  inherit (builtins)
    attrNames
    concatMap
    concatStringsSep
    elem
    elemAt
    filter
    hasAttr
    head
    isAttrs
    listToAttrs
    map
    match
    readDir
    substring;

  argsWithPath = args: parts:
    let meta.locatedAt = parts;
    in meta // (if isAttrs args then args else args meta);

  readDirVisible = path:
    let
      children = readDir path;
      # skip hidden files, except for those that contain special instructions to readTree
      isVisible = f: f == ".skip-subtree" || f == ".skip-tree" || (substring 0 1 f) != ".";
      names = filter isVisible (attrNames children);
    in
    listToAttrs (map
      (name: {
        inherit name;
        value = children.${name};
      })
      names);

  # Create a mark containing the location of this attribute and
  # a list of all child attribute names added by readTree.
  marker = parts: children: {
    __readTree = parts;
    __readTreeChildren = builtins.attrNames children;
  };

  # Create a label from a target's tree location.
  mkLabel = target:
    let label = concatStringsSep "/" target.__readTree;
    in if target ? __subtarget
    then "${label}:${target.__subtarget}"
    else label;

  # Merge two attribute sets, but place attributes in `passthru` via
  # `overrideAttrs` for derivation targets that support it.
  merge = a: b:
    if a ? overrideAttrs
    then
      a.overrideAttrs
        (prev: {
          passthru = (prev.passthru or { }) // b;
        })
    else a // b;

  # Like `builtins.scopedImport scopedArgs`, but ensures that `scopedArgs` is
  # also applied to any subsequent imports (scoped or otherwise) performed by
  # the imported files (and so on).
  #
  # Note that changes to scopedImport and import in scopedArgs are silently
  # ignored.
  propagatingScopedImport = scopedArgs:
    let
      propagatingScopedArgs = scopedArgs // {
        import = propagatingScopedImport scopedArgs;
        scopedImport = scopedArgs': propagatingScopedImport (scopedArgs // scopedArgs');

        builtins = scopedArgs.builtins or builtins // {
          import = propagatingScopedArgs.import;
          scopedImport = propagatingScopedArgs.scopedImport;
        };
      };
    in
      /* We could allow the user to change the import implementation using
       `scopedArgs.builtins.scopedImport or builtins.scopedImport`,
       but what would be the use case?
      */
    builtins.scopedImport propagatingScopedArgs;

  # Import a file and enforce our calling convention
  importFile = args: scopedArgs: path: parts: filter:
    let
      importedFile =
        if scopedArgs != { } && builtins ? scopedImport # For tvix
        then propagatingScopedImport scopedArgs path
        else import path;
      pathType = builtins.typeOf importedFile;
    in
    if pathType != "lambda"
    then throw "readTree: trying to import ${toString path}, but it’s a ${pathType}, you need to make it a function like { depot, pkgs, ... }"
    else importedFile (filter parts (argsWithPath args parts));

  nixFileName = file:
    let res = match "(.*)\\.nix" file;
    in if res == null then null else head res;

  # Internal implementation of readTree, which handles things like the
  # skipping of trees and subtrees.
  #
  # This method returns an attribute sets with either of two shapes:
  #
  # { ok = ...; }    # a tree was read successfully
  # { skip = true; } # a tree was skipped
  #
  # The higher-level `readTree` method assembles the final attribute
  # set out of these results at the top-level, and the internal
  # `children` implementation unwraps and processes nested trees.
  readTreeImpl = { args, initPath, rootDir, parts, argsFilter, scopedArgs }:
    let
      dir = readDirVisible initPath;

      # Determine whether any part of this tree should be skipped.
      #
      # Adding a `.skip-subtree` file will still allow the import of
      # the current node's "default.nix" file, but stop recursion
      # there.
      #
      # Adding a `.skip-tree` file will completely ignore the folder
      # in which this file is located.
      skipTree = hasAttr ".skip-tree" dir;
      skipSubtree = skipTree || hasAttr ".skip-subtree" dir;

      joinChild = c: initPath + ("/" + c);

      self =
        if rootDir
        then { __readTree = [ ]; }
        else importFile args scopedArgs initPath parts argsFilter;

      # Import subdirectories of the current one, unless any skip
      # instructions exist.
      #
      # This file can optionally contain information on why the tree
      # should be ignored, but its content is not inspected by
      # readTree
      filterDir = f: dir."${f}" == "directory";
      filteredChildren = map
        (c: {
          name = c;
          value = readTreeImpl {
            inherit argsFilter scopedArgs;
            args = args;
            initPath = (joinChild c);
            rootDir = false;
            parts = (parts ++ [ c ]);
          };
        })
        (filter filterDir (attrNames dir));

      # Remove skipped children from the final set, and unwrap the
      # result set.
      children =
        if skipSubtree then [ ]
        else map ({ name, value }: { inherit name; value = value.ok; }) (filter (child: child.value ? ok) filteredChildren);

      # Import Nix files
      nixFiles =
        if skipSubtree then [ ]
        else filter (f: f != null) (map nixFileName (attrNames dir));
      nixChildren = map
        (c:
          let
            p = joinChild (c + ".nix");
            childParts = parts ++ [ c ];
            imported = importFile args scopedArgs p childParts argsFilter;
          in
          {
            name = c;
            value =
              if isAttrs imported
              then merge imported (marker childParts { })
              else imported;
          })
        nixFiles;

      nodeValue = if dir ? "default.nix" then self else { };

      allChildren = listToAttrs (
        if dir ? "default.nix"
        then children
        else nixChildren ++ children
      );

    in
    if skipTree
    then { skip = true; }
    else {
      ok =
        if isAttrs nodeValue
        then merge nodeValue (allChildren // (marker parts allChildren))
        else nodeValue;
    };

  # Top-level implementation of readTree itself.
  readTree = args:
    let
      tree = readTreeImpl args;
    in
    if tree ? skip
    then throw "Top-level folder has a .skip-tree marker and could not be read by readTree!"
    else tree.ok;

  # Helper function to fetch subtargets from a target. This is a
  # temporary helper to warn on the use of the `meta.targets`
  # attribute, which is deprecated in favour of `meta.ci.targets`.
  subtargets = node:
    let targets = (node.meta.targets or [ ]) ++ (node.meta.ci.targets or [ ]);
    in if node ? meta.targets then
      builtins.trace ''
        [1;31mWarning: The meta.targets attribute is deprecated.

        Please move the subtargets of //${mkLabel node} to the
        meta.ci.targets attribute.
        [0m
      ''
        targets else targets;

  # Function which can be used to find all readTree targets within an
  # attribute set.
  #
  # This function will gather physical targets, that is targets which
  # correspond directly to a location in the repository, as well as
  # subtargets (specified in the meta.ci.targets attribute of a node).
  #
  # This can be used to discover targets for inclusion in CI
  # pipelines.
  #
  # Called with the arguments:
  #
  #   eligible: Function to determine whether the given derivation
  #             should be included in the build.
  gather = eligible: node:
    if node ? __readTree then
    # Include the node itself if it is eligible.
      (if eligible node then [ node ] else [ ])
      # Include eligible children of the node
      ++ concatMap (gather eligible) (map (attr: node."${attr}") node.__readTreeChildren)
      # Include specified sub-targets of the node
      ++ filter eligible (map
        (k: (node."${k}" or { }) // {
          # Keep the same tree location, but explicitly mark this
          # node as a subtarget.
          __readTree = node.__readTree;
          __readTreeChildren = [ ];
          __subtarget = k;
        })
        (subtargets node))
    else [ ];

  # Determine whether a given value is a derivation.
  # Copied from nixpkgs/lib for cases where lib is not available yet.
  isDerivation = x: isAttrs x && x ? type && x.type == "derivation";
in
{
  inherit gather mkLabel;

  __functor = _:
    { path
    , args
    , filter ? (_parts: x: x)
    , scopedArgs ? { }
    , rootDir ? true
    }:
    readTree {
      inherit args scopedArgs rootDir;
      argsFilter = filter;
      initPath = path;
      parts = [ ];
    };

  # In addition to readTree itself, some functionality is exposed that
  # is useful for users of readTree.

  # Create a readTree filter disallowing access to the specified
  # top-level folder in the repository, except for specific exceptions
  # specified by their (full) paths.
  #
  # Called with the arguments:
  #
  #   folder: Name of the restricted top-level folder (e.g. 'experimental')
  #
  #   exceptions: List of readTree parts (e.g. [ [ "services" "some-app" ] ]),
  #               which should be able to access the restricted folder.
  #
  #   reason: Textual explanation for the restriction (included in errors)
  restrictFolder = { folder, exceptions ? [ ], reason }: parts: args:
    if (elemAt parts 0) == folder || elem parts exceptions
    then args
    else args // {
      depot = args.depot // {
        "${folder}" = throw ''
          Access to targets under //${folder} is not permitted from
          other repository paths. Specific exceptions are configured
          at the top-level.

          ${reason}
          At location: ${builtins.concatStringsSep "." parts}
        '';
      };
    };

  # This definition of fix is identical to <nixpkgs>.lib.fix, but is
  # provided here for cases where readTree is used before nixpkgs can
  # be imported.
  #
  # It is often required to create the args attribute set.
  fix = f: let x = f x; in x;

  # Takes an attribute set and adds a meta.ci.targets attribute to it
  # which contains all direct children of the attribute set which are
  # derivations.
  #
  # Type: attrs -> attrs
  drvTargets = attrs:
    attrs // {
      # preserve .meta from original attrs
      meta = (attrs.meta or { }) // {
        # preserve .meta.ci (except .targets) from original attrs
        ci = (attrs.meta.ci or { }) // {
          targets = builtins.filter
            (x: isDerivation attrs."${x}")
            (builtins.attrNames attrs);
        };
      };
    };

  # Exposed because it is reusable (and for the test suite).
  # See above for documentation.
  inherit propagatingScopedImport;
}
