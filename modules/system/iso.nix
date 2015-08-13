{ lib, ... }:

{
  options.vuizvui.createISO = lib.mkOption {
    default = false;
    example = true;
    type = lib.types.bool;
    description = ''
      Whether to build an ISO image out of this machine configuration on Hydra.
    '';
  };
}
