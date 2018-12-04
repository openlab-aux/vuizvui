{ config, pkgs, lib, ... }:

let
  kernelVersion = config.boot.kernelPackages.kernel.version;
  hasZstd = lib.versionAtLeast kernelVersion "4.18";
in {
  options.vuizvui.system.kernel.zswap.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = ''
      Whether to enable support for zswap with <literal>z3fold</literal> for
      pooling and <literal>zstd</literal> for compression, if available
      (otherwise it falls back to <literal>lzo</literal>).

      Zswap is a compressed cache for swap pages, which is especially useful
      for machines with limited RAM.
    '';
  };

  config = lib.mkIf config.vuizvui.system.kernel.zswap.enable {
    boot.kernelPatches = lib.singleton {
      name = "zswap-config";
      patch = null;
      extraConfig = ''
        CRYPTO_${if hasZstd then "ZSTD" else "LZO"} y
        ZSWAP y
        Z3FOLD y
      '';
    };

    boot.kernelParams = [
      "zswap.enabled=1"
      "zswap.zpool=z3fold"
      "zswap.compressor=${if hasZstd then "zstd" else "lzo"}"
    ];
  };
}
