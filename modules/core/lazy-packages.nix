{ config, pkgs, lib, ... }:

let
  inherit (lib) escapeShellArg;
  inherit (config.vuizvui) lazyPackages;

  vuizvuiWrapper = package: pkgs.vuizvui.lazy-packages.mkWrapper {
    inherit package;
    extraErrorMessage = "Please remove `${escapeShellArg package.name}' from `vuizvui.lazyPackages'.";
  };

  wrappers = map vuizvuiWrapper config.vuizvui.lazyPackages;

in {
  options.vuizvui.lazyPackages = lib.mkOption {
    type = lib.types.listOf lib.types.package;
    default = [];
    example = lib.literalExample "[ pkgs.gimp pkgs.libreoffice ]";
    description = ''
      Packages which are built for this system but instead of being a full
      runtime dependency, only wrappers of all executables that reside in the
      <literal>bin</literal> directory are actually runtime dependencies.

      As soon as one of these wrappers is executed, the real package is fetched
      and the corresponding binary is executed.
    '';
  };

  config.environment.systemPackages = map vuizvuiWrapper lazyPackages;
}
