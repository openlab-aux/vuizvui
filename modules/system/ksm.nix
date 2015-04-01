{ config, lib, ... }:

with lib;

{
  options.vuizvui.system.enableKSM = mkEnableOption "Kernel Same-Page Merging";

  config = mkIf config.vuizvui.system.enableKSM {
    systemd.services.enable-ksm = {
      description = "Enable Kernel Same-Page Merging";
      wantedBy = [ "multi-user.target" ];
      after = [ "systemd-udev-settle.service" ];
      script = ''
        if [ -e /sys/kernel/mm/ksm ]; then
          echo 1 > /sys/kernel/mm/ksm/run
        fi
      '';
    };
  };
}
