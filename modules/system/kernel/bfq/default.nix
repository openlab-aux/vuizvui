{ config, lib, ... }:

let
  inherit (config.boot.kernelPackages.kernel) version;
  inherit (lib) optionalString versionAtLeast versionOlder;
in {
  options.vuizvui.system.kernel.bfq = {
    enable = lib.mkEnableOption "Enable the BFQ scheduler by default";
  };

  config = lib.mkIf config.vuizvui.system.kernel.bfq.enable {
    boot.kernelPatches = lib.singleton {
      name = "bfq";
      patch = ./bfq-by-default.patch;
      extraConfig = ''
        SCSI_MQ_DEFAULT y
        DM_MQ_DEFAULT y
        IOSCHED_BFQ y
        BFQ_GROUP_IOSCHED y
      '';
    };

    vuizvui.requiresTests = lib.singleton ["vuizvui" "system" "kernel" "bfq"];

    assertions = lib.singleton {
      assertion = versionAtLeast version "4.12";

      message = "The BFQ scheduler in conjunction with blk-mq requires "
              + "at least kernel 4.12.";
    };
  };
}
