{ config, pkgs, lib, ... }:

with lib;

let
  whichNet = if config.networking.useNetworkd then "networkd" else "scripted";

  mkTest = attrs: if attrs.check then attrs.paths or [ attrs.path ] else [];

  anyAttrs = pred: cfg: any id (mapAttrsToList (const pred) cfg);

  upstreamTests = concatMap mkTest [
    { check = config.services.avahi.enable;
      path  = ["nixos" "avahi"];
    }
    { check = config.vuizvui.createISO;
      paths = [
        ["nixos" "bootBiosCdrom"]
        ["nixos" "bootBiosUsb"]
        ["nixos" "bootUefiCdrom"]
        ["nixos" "bootUefiUsb"]
      ];
    }
    { check = config.services.cadvisor.enable;
      path  = ["nixos" "cadvisor"];
    }
    { check = config.services.cjdns.enable;
      path  = ["nixos" "cjdns"];
    }
    { check = config.containers != {};
      path  = ["nixos" "containers"];
    }
    { check = config.virtualisation.docker.enable;
      path  = ["nixos" "docker"];
    }
    { check = config.services.dockerRegistry.enable;
      path  = ["nixos" "dockerRegistry"];
    }
    { check = config.services.etcd.enable;
      path  = ["nixos" "etcd"];
    }
    { check = config.networking.firewall.enable;
      path  = ["nixos" "firewall"];
    }
    { check = config.services.fleet.enable;
      path  = ["nixos" "fleet"];
    }
    { check = config.services.xserver.desktopManager.gnome3.enable;
      path  = ["nixos" "gnome3"];
    }
    { check = config.services.xserver.displayManager.gdm.enable;
      path  = ["nixos" "gnome3-gdm"];
    }
    { check = config.services.xserver.windowManager.i3.enable;
      path  = ["nixos" "i3wm"];
    }
    { check = elem "btrfs" config.boot.supportedFilesystems;
      paths = [
        ["nixos" "installer" "btrfsSimple"]
        ["nixos" "installer" "btrfsSubvols"]
        ["nixos" "installer" "btrfsSubvolDefault"]
      ];
    }
    { check = config.boot.loader.grub.version == 1;
      path  = ["nixos" "installer" "grub1"];
    }
    { check = config.boot.initrd.luks.devices != [];
      path  = ["nixos" "installer" "luksroot"];
    }
    { check = true;
      path  = ["nixos" "installer" "lvm"];
    }
    { check = config.fileSystems ? "/boot";
      path  = ["nixos" "installer" "separateBoot"];
    }
    { check = config.fileSystems ? "/boot"
           && config.fileSystems."/boot".fsType == "vfat";
      path  = ["nixos" "installer" "separateBootFat"];
    }
    { check = elem "ext3" config.boot.supportedFilesystems;
      path  = ["nixos" "installer" "simple"];
    }
    { check = config.boot.loader.grub.fsIdentifier == "label";
      path  = ["nixos" "installer" "simpleLabels"];
    }
    { check = config.boot.loader.grub.fsIdentifier == "provided";
      path  = ["nixos" "installer" "simpleProvided"];
    }
    { check = config.boot.initrd.mdadmConf != "";
      path  = ["nixos" "installer" "swraid"];
    }
    { check = config.services.influxdb.enable;
      path  = ["nixos" "influxdb"];
    }
    { check = config.networking.enableIPv6;
      path  = ["nixos" "ipv6"];
    }
    { check = config.services.jenkins.enable;
      path  = ["nixos" "jenkins"];
    }
    { check = config.services.xserver.desktopManager.kde4.enable;
      path  = ["nixos" "kde4"];
    }
    { check = with config.services.kubernetes; apiserver.enable
           || scheduler.enable || controllerManager.enable || kubelet.enable
           || proxy.enable;
      path  = ["nixos" "kubernetes"];
    }
    { check = config.boot.kernelPackages.kernel.version
           == pkgs.linuxPackages_latest.kernel.version;
      path  = ["nixos" "latestKernel" "login"];
    }
    { check = true;
      path  = ["nixos" "login"];
    }
    { check = true;
      path  = ["nixos" "misc"];
    }
    { check = config.services.murmur.enable;
      path  = ["nixos" "mumble"];
    }
    { check = config.services.munin-node.enable
           || config.services.munin-cron.enable;
      path  = ["nixos" "munin"];
    }
    { check = config.services.mysql.enable;
      path  = ["nixos" "mysql"];
    }
    { check = config.services.mysql.enable
           && config.services.mysql.replication.role != "none";
      path  = ["nixos" "mysqlReplication"];
    }
    { check = config.networking.nat.enable
           && config.networking.firewall.enable;
      path  = ["nixos" "nat" "firewall"];
    }
    { check = config.networking.nat.enable
           && !config.networking.firewall.enable;
      path  = ["nixos" "nat" "standalone"];
    }
    { check = config.networking.bonds != {};
      path  = ["nixos" "networking" whichNet "bond"];
    }
    { check = config.networking.bridges != {};
      path  = ["nixos" "networking" whichNet "bridge"];
    }
    { check = anyAttrs (i: i.useDHCP == true) config.networking.interfaces;
      path  = ["nixos" "networking" whichNet "dhcpOneIf"];
    }
    { check = config.networking.useDHCP;
      path  = ["nixos" "networking" whichNet "dhcpSimple"];
    }
    { check = true;
      path  = ["nixos" "networking" whichNet "loopback"];
    }
    { check = config.networking.macvlans != {};
      path  = ["nixos" "networking" whichNet "macvlan"];
    }
    { check = config.networking.sits != {};
      path  = ["nixos" "networking" whichNet "sit"];
    }
    { check = anyAttrs (i: i.ip4 != []) config.networking.interfaces;
      path  = ["nixos" "networking" whichNet "static"];
    }
    { check = config.networking.vlans != {};
      path  = ["nixos" "networking" whichNet "vlan"];
    }
    { check = with config.networking.proxy; any (val: val != null)
            [ default allProxy ftpProxy httpProxy httpsProxy noProxy
              rsyncProxy
            ];
      path  = ["nixos" "networkingProxy"];
    }
    { check = elem "nfs" config.boot.supportedFilesystems;
      paths = [
        ["nixos" "nfs3"]
        ["nixos" "nfs4"]
      ];
    }
    { check = true;
      path  = ["nixos" "nixosPinVersion"];
    }
    { check = config.services.nsd.enable;
      path  = ["nixos" "nsd"];
    }
    { check = config.services.openssh.enable;
      path  = ["nixos" "openssh"];
    }
    { check = config.services.panamax.enable;
      path  = ["nixos" "panamax"];
    }
    { check = config.services.peerflix.enable;
      path  = ["nixos" "peerflix"];
    }
    { check = config.services.printing.enable;
      path  = ["nixos" "printing"];
    }
    { check = config.services.httpd.enable
           && elem "proxy_balancer" config.services.httpd.extraModules;
      path  = ["nixos" "proxy"];
    }
    { check = config.services.pumpio.enable;
      path  = ["nixos" "pumpio"];
    }
    { check = config.hardware.opengl.driSupport
           && config.services.xserver.enable;
      path  = ["nixos" "quake3"];
    }
    { check = true;
      path  = ["nixos" "runInMachine"];
    }
    { check = config.services.xserver.displayManager.sddm.enable;
      path  = ["nixos" "sddm"];
    }
    { check = true;
      path  = ["nixos" "simple"];
    }
    { check = config.services.tomcat.enable;
      path  = ["nixos" "tomcat"];
    }
    { check = config.services.udisks2.enable;
      path  = ["nixos" "udisks2"];
    }
    { check = config.virtualisation.virtualbox.host.enable;
      path  = ["nixos" "virtualbox"];
    }
    { check = config.services.xserver.desktopManager.xfce.enable;
      path  = ["nixos" "xfce"];
    }
  ];

in {
  options.vuizvui = {
    requiresTests = mkOption {
      type = types.listOf (types.listOf types.str);
      default = [];
      example = [ ["nixos" "nat" "firewall"] ["vuizvui" "foo"] ];
      description = ''
        A list of attribute paths to the tests which need to succeed in order to
        trigger a channel update for the current configuration/machine.

        Every attribute path itself is a list of attribute names, which are
        queried using <function>lib.getAttrFromPath</function>.
      '';
    };
  };

  config.vuizvui.requiresTests = upstreamTests;
}
