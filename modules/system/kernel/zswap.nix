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
      # This patch is needed until it hits stable/mainline to prevent z3fold
      # crashes.
      #
      # See also: https://bugs.chromium.org/p/chromium/issues/detail?id=822360
      patch = pkgs.fetchpatch {
        name = "z3fold-fix-wrong-handling-of-headless-pages.patch";
        url = "https://patchwork.kernel.org/patch/10510583/raw/";
        sha256 = "0c9l912zgxwy31b7m4xkf31imzvjs11n1i6v5w2sykqfx3sk6a3b";
      };
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
