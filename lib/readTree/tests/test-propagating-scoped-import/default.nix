{ propagatingScopedImport }:

propagatingScopedImport
{
  constant = 5;

  builtins = builtins // {
    add = x: y: x - y;
  };
} ./entry.nix
