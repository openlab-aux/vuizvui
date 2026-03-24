# The source of the homeRepo (kot/Profpatsch), fetched as a plain store path.
# Using builtins.fetchGit so this can be imported without pkgs.
# Update rev/narHash whenever the homeRepo pin is bumped.
builtins.fetchGit {
  url = "https://codeberg.org/Profpatsch/Profpatsch.git";
  allRefs = true;
  rev = "699607c1215fdd72a2de51cbcf0ff57cf6cc3fba";
}
