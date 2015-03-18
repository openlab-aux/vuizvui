{
  nixpkgs.config.packageOverrides = pkgs: {
    labernix = import ../../pkgs {
      inherit pkgs;
    };
  };
}
