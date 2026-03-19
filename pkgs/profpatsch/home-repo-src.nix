# The source of the homeRepo (kot/Profpatsch), fetched as a plain store path.
# Using builtins.fetchGit so this can be imported without pkgs.
# Update rev/narHash whenever the homeRepo pin is bumped.
builtins.fetchGit {
  url = "https://codeberg.org/Profpatsch/Profpatsch.git";
  rev = "27597cc6b49e69e6186c64fcb4d231c4c5402cc3";
}
