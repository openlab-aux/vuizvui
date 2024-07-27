let
  path = builtins.tryEval <nixos-hardware>;

  pin = builtins.fetchGit {
    name = "nixos-hardware";
    url = "https://github.com/NixOS/nixos-hardware";
    # 2024-07-27
    rev = "0b4d40f95a68ef0a6785f6b938ac8c1383321dbf";
    ref = "master";
  };

# use nixos-hardware from NIX_PATH, otherwise use the pinned verison
in toString
  (if path.success
  then path.value
  else pin)
