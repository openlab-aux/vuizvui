# The source of the homeRepo (kot/Profpatsch), fetched as a plain store path.
# Using builtins.fetchGit so this can be imported without pkgs.
# Update rev/narHash whenever the homeRepo pin is bumped.
builtins.fetchGit {
  url = "https://codeberg.org/Profpatsch/Profpatsch.git";
  allRefs = true;
  rev = "76a6f95cfbbdb0a7745589a7a8e1c03ebebd3934";
}
