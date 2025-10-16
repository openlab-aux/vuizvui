readTree
========

This is a Nix program that builds up an attribute set tree for a large
repository based on the filesystem layout.

It is in fact the tool that lays out the attribute set of this repository.

As an example, consider a root (`.`) of a repository and a layout such as:

```
.
├── third_party
│   ├── default.nix
│   └── rustpkgs
│       ├── aho-corasick.nix
│       └── serde.nix
└── tools
    ├── cheddar
    │   └── default.nix
    └── roquefort.nix
```

When `readTree` is called on that tree, it will construct an attribute set with
this shape:

```nix
{
    tools = {
        cheddar = ...;
        roquefort = ...;
    };

    third_party = {
        # the `default.nix` of this folder might have had arbitrary other
        # attributes here, such as this:
        favouriteColour = "orange";

        rustpkgs = {
            aho-corasick = ...;
            serde = ...;
        };
    };
}
```

Every imported Nix file that yields an attribute set will have a `__readTree =
true;` attribute merged into it.

## Traversal logic

`readTree` will follow any subdirectories of a tree and import all Nix files,
with some exceptions:

* If a folder contains a `default.nix` file, no *sibling* Nix files will be
  imported - however children are traversed as normal.
* If a folder contains a `default.nix` it is loaded and, if it
  evaluates to a set, *merged* with the children. If it evaluates to
  anything other than a set, else the children are *not traversed*.
* A folder can opt out from readTree completely by containing a
  `.skip-tree` file. The content of the file is not read. These
  folders will be missing completely from the readTree structure.
* A folder can declare that its children are off-limit by containing a
  `.skip-subtree` file. Since the content of the file is not checked, it can be
  useful to leave a note for a human in the file.
* The `default.nix` of the top-level folder on which readTree is
  called is **not** read to avoid infinite recursion (as, presumably,
  this file is where readTree itself is called).

Traversal is lazy, `readTree` will only build up the tree as requested. This
currently has the downside that directories with no importable files end up in
the tree as empty nodes (`{}`).

## Import structure

`readTree` is called with an argument set containing a few parameters:

* `path`: Initial path at which to start the traversal.
* `args`: Arguments to pass to all imports.
* `filter`: (optional) A function to filter the argument set on each
  import based on the location in the tree. This can be used to, for
  example, implement a "visibility" system inside of a tree.
* `scopedArgs`: (optional) An argument set that is passed to all
  imported files via `builtins.scopedImport`. This will forcefully
  and recursively override the given values in the import scope, use
  with care!

The package headers in this repository follow the form `{ pkgs, ... }:` where
`pkgs` is a fixed-point of the entire package tree (see the `default.nix` at the
root of the depot).

In theory `readTree` can pass arguments of different shapes, but I have found
this to be a good solution for the most part.

Note that `readTree` does not currently make functions overridable, though it is
feasible that it could do that in the future.
