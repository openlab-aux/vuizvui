# Select modules from module-list.nix based on prefix matching
#
# Usage:
#   selectModules [ "./user/profpatsch/" "./core/" "./services/guix.nix" ]
#
# Returns: List of module paths that match any of the given prefixes

prefixes:

let
  lib = import <nixpkgs/lib>;
  allModules = import ../modules/module-list.nix;

  # Get the absolute path to the modules directory
  modulesDir = toString ../modules;

  # Check if a module path matches any of the given prefixes
  matchesAnyPrefix = modulePath:
    let
      # Convert to absolute path string
      moduleAbsPath = toString modulePath;
      # Extract the relative part after the modules directory
      # e.g., "/path/to/modules/core/common.nix" -> "core/common.nix"
      relPath = lib.removePrefix (modulesDir + "/") moduleAbsPath;
    in
      lib.any (prefix:
        # Remove leading ./ from prefix for comparison
        let cleanPrefix = lib.removePrefix "./" prefix;
        in lib.hasPrefix cleanPrefix relPath
      ) prefixes;

in
  builtins.filter matchesAnyPrefix allModules
