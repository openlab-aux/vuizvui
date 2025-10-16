{ depot, lib, ... }:

let
  inherit (depot.nix.runTestsuite)
    runTestsuite
    it
    assertEq
    assertThrows
    ;

  tree-ex = depot.nix.readTree {
    path = ./test-example;
    args = { };
  };

  example = it "corresponds to the README example" [
    (assertEq "third_party attrset"
      (lib.isAttrs tree-ex.third_party
        && (! lib.isDerivation tree-ex.third_party))
      true)
    (assertEq "third_party attrset other attribute"
      tree-ex.third_party.favouriteColour
      "orange")
    (assertEq "rustpkgs attrset aho-corasick"
      tree-ex.third_party.rustpkgs.aho-corasick
      "aho-corasick")
    (assertEq "rustpkgs attrset serde"
      tree-ex.third_party.rustpkgs.serde
      "serde")
    (assertEq "tools cheddear"
      "cheddar"
      tree-ex.tools.cheddar)
    (assertEq "tools roquefort"
      tree-ex.tools.roquefort
      "roquefort")
  ];

  tree-tl = depot.nix.readTree {
    path = ./test-tree-traversal;
    args = { };
  };

  traversal-logic = it "corresponds to the traversal logic in the README" [
    (assertEq "skip-tree/a is read"
      tree-tl.skip-tree.a
      "a is read normally")
    (assertEq "skip-tree does not contain b"
      (builtins.attrNames tree-tl.skip-tree)
      [ "__readTree" "__readTreeChildren" "a" ])
    (assertEq "skip-tree children list does not contain b"
      tree-tl.skip-tree.__readTreeChildren
      [ "a" ])

    (assertEq "skip subtree default.nix is read"
      tree-tl.skip-subtree.but
      "the default.nix is still read")
    (assertEq "skip subtree a/default.nix is skipped"
      (tree-tl.skip-subtree ? a)
      false)
    (assertEq "skip subtree b/c.nix is skipped"
      (tree-tl.skip-subtree ? b)
      false)
    (assertEq "skip subtree a/default.nix would be read without .skip-subtree"
      (tree-tl.no-skip-subtree.a)
      "am I subtree yet?")
    (assertEq "skip subtree b/c.nix would be read without .skip-subtree"
      (tree-tl.no-skip-subtree.b.c)
      "cool")

    (assertEq "default.nix attrset is merged with siblings"
      tree-tl.default-nix.no
      "siblings should be read")
    (assertEq "default.nix means sibling isn’t read"
      (tree-tl.default-nix ? sibling)
      false)
    (assertEq "default.nix means subdirs are still read and merged into default.nix"
      (tree-tl.default-nix.subdir.a)
      "but I’m picked up")

    (assertEq "default.nix can be not an attrset"
      tree-tl.default-nix.no-merge
      "I’m not merged with any children")
    (assertEq "default.nix is not an attrset -> children are not merged"
      (tree-tl.default-nix.no-merge ? subdir)
      false)

    (assertEq "default.nix can contain a derivation"
      (lib.isDerivation tree-tl.default-nix.can-be-drv)
      true)
    (assertEq "Even if default.nix is a derivation, children are traversed and merged"
      tree-tl.default-nix.can-be-drv.subdir.a
      "Picked up through the drv")
    (assertEq "default.nix drv is not changed by readTree"
      tree-tl.default-nix.can-be-drv
      (import ./test-tree-traversal/default-nix/can-be-drv/default.nix { }))
  ];

  # these each call readTree themselves because the throws have to happen inside assertThrows
  wrong = it "cannot read these files and will complain" [
    (assertThrows "this file is not a function"
      (depot.nix.readTree {
        path = ./test-wrong-not-a-function;
        args = { };
      }).not-a-function)
    # can’t test for that, assertThrows can’t catch this error
    # (assertThrows "this file is a function but doesn’t have dots"
    #   (depot.nix.readTree {} ./test-wrong-no-dots).no-dots-in-function)
  ];

  read-markers = depot.nix.readTree {
    path = ./test-marker;
    args = { };
  };

  assertMarkerByPath = path:
    assertEq "${lib.concatStringsSep "." path} is marked correctly"
      (lib.getAttrFromPath path read-markers).__readTree
      path;

  markers = it "marks nodes correctly" [
    (assertMarkerByPath [ "directory-marked" ])
    (assertMarkerByPath [ "directory-marked" "nested" ])
    (assertMarkerByPath [ "file-children" "one" ])
    (assertMarkerByPath [ "file-children" "two" ])
    (assertEq "nix file children are marked correctly"
      read-markers.file-children.__readTreeChildren [ "one" "two" ])
    (assertEq "directory children are marked correctly"
      read-markers.directory-marked.__readTreeChildren [ "nested" ])
    (assertEq "absence of children is marked"
      read-markers.directory-marked.nested.__readTreeChildren [ ])
  ];

  propagating-scoped-import =
    let
      result = import ./test-propagating-scoped-import {
        inherit (depot.nix.readTree) propagatingScopedImport;
      };
    in
    it "propagates scopedArgs correctly" [
      (assertEq "global gets added correctly" result.constant 5)
      (assertEq "builtins.add gets changed correctly"
        result.double-constant
        (5 - 5))
      (assertEq "scopedArgs get propagated across import"
        result.transitive-import
        (28 - 5))
      (assertEq "unchanged scopedArgs get propagated across scopedImport"
        result.transitive-scoped-import
        (28 + 5))
      (assertEq "unchanged scopedArgs get propagated across two imports"
        result.transitive-import-importing [
        null
        (28 - 5)
        (28 - 42)
        5
        (5 * 2 - 5)
      ])
      (assertEq "changed scopedArgs get propagated across two imports" result.transitive-scoped-import-importing [
        "null"
        (28 * 5)
        (28 * 42)
        5
        (5 * 2 * 5)
      ])
      (assertEq "builtins set is correct if scopedArgs.builtins = { }"
        result.empty-ish-builtins
        [ "import" "scopedImport" ])
      (assertEq "all default builtins are present if !(scopedArgs ? builtins)"
        result.have-all-default-builtins
        true)
    ];
in
runTestsuite "readTree" [
  example
  traversal-logic
  wrong
  markers
  propagating-scoped-import
]
