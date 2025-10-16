[
  null
  (import ./transitive.nix)
  (scopedImport
    {
      constant = 42;
    } ./transitive.nix)
  constant
  (builtins.add (2 * constant) constant)
]
