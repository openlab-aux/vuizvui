{
  inherit constant;
  double-constant = builtins.add constant constant;

  transitive-import = builtins.import ./transitive.nix;
  transitive-scoped-import = builtins.scopedImport
    {
      builtins = builtins // {
        add = x: y: x + y;
      };
    } ./transitive.nix;

  transitive-import-importing = import ./transitive-import.nix;
  transitive-scoped-import-importing = scopedImport
    {
      null = "null";
      builtins = builtins // {
        add = x: y: x * y;
      };
    } ./transitive-import.nix;

  empty-ish-builtins = builtins.scopedImport
    {
      inherit (builtins) attrNames;
      builtins = { };
    } ./builtins-attr-names.nix;

  have-all-default-builtins = builtins.scopedImport
    {
      defaultBuiltins = builtins;
    } ./have-all-builtins.nix;
}
