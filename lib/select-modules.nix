# Select modules from module-list.nix based on prefix matching
#
# Usage:
#   selectModules [ "./user/profpatsch/" "./core/" "./services/guix.nix" ]
#
# Returns: List of module paths that match any of the given prefixes

prefixes:

let
  hasPrefix = p: s:
    builtins.substring 0 (builtins.stringLength p) s == p;
  removePrefix = p: s:
    if !(hasPrefix p s) then s else builtins.substring (builtins.stringLength p) (-1) s;

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
      relPath = removePrefix (modulesDir + "/") moduleAbsPath;
    in
      builtins.any (prefix:
        # Remove leading ./ from prefix for comparison
        let cleanPrefix = removePrefix "./" prefix;
        in hasPrefix cleanPrefix relPath
      ) prefixes;

in
  builtins.filter matchesAnyPrefix allModules
