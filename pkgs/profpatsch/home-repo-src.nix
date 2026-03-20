# The source of the homeRepo (kot/Profpatsch), fetched as a plain store path.
# Using builtins.fetchGit so this can be imported without pkgs.
# Update rev/narHash whenever the homeRepo pin is bumped.
builtins.fetchGit {
  url = "https://codeberg.org/Profpatsch/Profpatsch.git";
  allRefs = true;
  rev = "cc44ab3ab7974e6665362828ce9f78a443f0b0fa";
}
