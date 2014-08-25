pkgs: kpkgs:

with pkgs.lib;

kpkgs // {
  virtualbox = overrideDerivation kpkgs.virtualbox (o: {
    patches = (o.patches or []) ++ (singleton (pkgs.fetchurl {
      name = "virtualbox-fix-linux-3.17.patch";
      url = "https://forums.virtualbox.org/download/file.php?id=13399";
      sha256 = "01ngbcdpc3h0ryvjzl2hdw5fp4141k9f56bqy8y6sh56mamyj9q7";
    }));
  });
}
