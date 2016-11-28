{ hlib, haskellPackages, fetchFromGitHub }:

let hp = haskellPackages.override {
  overrides = (self: super: {
    gitit = (hlib.overrideCabal super.gitit (drv: rec {
      src = fetchFromGitHub {
        owner = "openlab-aux";
        repo = "gitit";
        rev = "3da7c841f9382d0c62242a1b718511acec97e9f7";
        sha256 = "0qhkbvm4ixav4nln3m9845w9m3gzfq5xh4nxp2c9qj4w9p79if7z";
      };
      broken = true;
      platforms = [ "x86_64-linux" ];
      hydraPlatforms = platforms;
    }));
  });
};
in hp.gitit
