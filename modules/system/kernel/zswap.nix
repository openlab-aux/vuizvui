{ config, pkgs, lib, ... }:

let
  kernelVersion = config.boot.kernelPackages.kernel.version;
  hasZstd = lib.versionAtLeast kernelVersion "4.18";
in {
  options.vuizvui.system.kernel.zswap.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = ''
      Whether to enable support for zswap with `z3fold` for
      pooling and `zstd` for compression, if available
      (otherwise it falls back to `lzo`).

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
        ZSMALLOC y
      '';
    };

    boot.kernelParams = [
      "zswap.enabled=1"
      "zswap.zpool=zsmalloc"
      "zswap.compressor=${if hasZstd then "zstd" else "lzo"}"
    ];
  };
}
