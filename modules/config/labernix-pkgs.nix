{
  nixpkgs.config.overridePackages = pkgs: {
    labernix = import ../../pkgs {
      inherit pkgs;
    };
  };
}
