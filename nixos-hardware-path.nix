let
  path = builtins.tryEval <nixos-hardware>;

  pin = builtins.fetchGit {
    name = "nixos-hardware";
    url = "https://github.com/NixOS/nixos-hardware";
    # 2025-05-09
    rev = "3c5e12673265dfb0de3d9121420c0c2153bf21e0";
    ref = "master";
  };

# use nixos-hardware from NIX_PATH, otherwise use the pinned verison
in toString
  (if path.success
  then path.value
  else pin)
