# The source of the homeRepo (kot/Profpatsch), fetched as a plain store path.
# Using builtins.fetchGit so this can be imported without pkgs.
# Update rev/narHash whenever the homeRepo pin is bumped.
builtins.fetchGit {
  url = "https://codeberg.org/Profpatsch/Profpatsch.git";
  allRefs = true;
  rev = "2531a19f132f287c842d814a4907e9000fd88c2f";
}
