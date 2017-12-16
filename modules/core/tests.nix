{ options, config, pkgs, lib, ... }:

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
        ["nixos" "boot" "biosCdrom"]
        ["nixos" "boot" "biosUsb"]
        ["nixos" "boot" "netboot"]
        ["nixos" "boot" "uefiCdrom"]
        ["nixos" "boot" "uefiUsb"]
      ];
    }
    { check = true;
      path  = ["nixos" "boot-stage1"];
    }
    { check = config.services.cadvisor.enable;
      path  = ["nixos" "cadvisor"];
    }
    { check = config.services.cjdns.enable;
      path  = ["nixos" "cjdns"];
    }
    { check = config.services.cloud-init.enable;
      path  = ["nixos" "cloud-init"];
    }
    { check = config.boot.enableContainers
           && config.containers != {};
      path  = ["nixos" "containers"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.hostBridge != null) config.containers;
      path  = ["nixos" "containers-bridge"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.extraVeths != {}) config.containers;
      path  = ["nixos" "containers-extra_veth"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.localAddress != []) config.containers;
      path  = ["nixos" "containers-hosts"];
    }
    { check = config.boot.enableContainers;
      path  = ["nixos" "containers-imperative"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.hostAddress  != null
                        || i.localAddress != null) config.containers;
      path  = ["nixos" "containers-ipv4"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.hostAddress6  != null
                        || i.localAddress6 != null) config.containers;
      path  = ["nixos" "containers-ipv6"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.macvlans != []) config.containers;
      path  = ["nixos" "containers-macvlans"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.interfaces != []) config.containers;
      path  = ["nixos" "containers-physical_interfaces"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.privateNetwork) config.containers;
      path  = ["nixos" "containers-restart_networking"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.tmpfs != []) config.containers;
      path  = ["nixos" "containers-tmpfs"];
    }
    { check = config.services.dnscrypt-proxy.enable;
      path  = ["nixos" "dnscrypt-proxy"];
    }
    { check = config.virtualisation.docker.enable;
      path  = ["nixos" "docker"];
    }
    { check = config.security.pam.enableEcryptfs;
      path  = ["nixos" "ecryptfs"];
    }
    { check = config.services.etcd.enable;
      path  = ["nixos" "etcd"];
    }
    { check = config.services.ferm.enable;
      path  = ["nixos" "ferm"];
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
    { check = config.services.gocd-agent.enable;
      path  = ["nixos" "gocd-agent"];
    }
    { check = config.services.gocd-server.enable;
      path  = ["nixos" "gocd-server"];
    }
    { check = config.security.lockKernelModules
           || config.security.hideProcessInformation
           || config.boot.kernel.sysctl."user.max_user_namespaces" or 1 == 0;
      path  = ["nixos" "hardened"];
    }
    { check = true;
      path  = ["nixos" "hibernate"];
    }
    { check = config.services.hound.enable;
      path  = ["nixos" "hound"];
    }
    { check = config.services.xserver.windowManager.i3.enable;
      path  = ["nixos" "i3wm"];
    }
    { check = config.boot.initrd.network.enable;
      path  = ["nixos" "initrdNetwork"];
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
    { check = config.boot.loader.systemd-boot.enable;
      path  = ["nixos" "installer" "simpleUefiSystemdBoot"];
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
    { check = config.i18n.consoleKeyMap          == "azerty/fr"
           || config.services.xserver.layout     == "fr";
      path  = ["nixos" "keymap" "azerty"];
    }
    { check = config.i18n.consoleKeyMap          == "en-latin9"
           || config.services.xserver.xkbVariant == "colemak";
      path  = ["nixos" "keymap" "colemak"];
    }
    { check = config.i18n.consoleKeyMap          == "dvorak"
           || config.services.xserver.layout     == "dvorak";
      path  = ["nixos" "keymap" "dvorak"];
    }
    { check = config.i18n.consoleKeyMap          == "dvp"
           || config.services.xserver.xkbVariant == "dvp";
      path  = ["nixos" "keymap" "dvp"];
    }
    { check = config.i18n.consoleKeyMap          == "neo"
           || config.services.xserver.xkbVariant == "neo";
      path  = ["nixos" "keymap" "neo"];
    }
    { check = config.i18n.consoleKeyMap          == "de"
           || config.services.xserver.layout     == "de";
      path  = ["nixos" "keymap" "qwertz"];
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
    { check = config.services.openldap.enable;
      path  = ["nixos" "ldap"];
    }
    { check = config.services.leaps.enable;
      path  = ["nixos" "leaps"];
    }
    { check = true;
      path  = ["nixos" "login"];
    }
    { check = config.services.mathics.enable;
      path  = ["nixos" "mathics"];
    }
    { check = true;
      path  = ["nixos" "misc"];
    }
    { check = config.services.mongodb.enable;
      path  = ["nixos" "mongodb"];
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
    { check = with config.networking; let
        isIptables = nat.enable || firewall.enable;
        hasConntrack = firewall.connectionTrackingModules != []
                    || firewall.autoLoadConntrackHelpers;
      in isIptables && hasConntrack;
      path  = ["nixos" "nat" "firewall-conntrack"];
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
    { check = config.services.nginx.enable;
      path  = ["nixos" "nginx"];
    }
    { check = config.services.nsd.enable;
      path  = ["nixos" "nsd"];
    }
    { check = config.services.openssh.enable;
      path  = ["nixos" "openssh"];
    }
    { check = config.security.pam.oath.enable;
      path  = ["nixos" "pam-oath-login"];
    }
    { check = config.services.peerflix.enable;
      path  = ["nixos" "peerflix"];
    }
    { check = config.services.postgresql.enable
           && elem pkgs.pgjwt config.services.postgresql.extraPlugins;
      path  = ["nixos" "pgjwt"];
    }
    { check = config.services.xserver.desktopManager.plasma5.enable;
      path  = ["nixos" "plasma5"];
    }
    { check = config.services.postgresql.enable;
      path  = let
        filterPg = name: drv: hasPrefix "postgresql" name
                           && drv == config.services.postgresql.package;
        pgPackage = head (attrNames (filterAttrs filterPg pkgs));
      in ["nixos" "postgresql" pgPackage];
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
    # TODO: The upstream service was disabled because it broke
    # { check = config.services.quagga.ospf.enable;
    #   path  = ["nixos" "quagga"];
    # }
    { check = config.hardware.opengl.driSupport
           && config.services.xserver.enable;
      path  = ["nixos" "quake3"];
    }
    { check = true;
      path  = ["nixos" "runInMachine"];
    }
    { check = config.services.samba.enable;
      path  = ["nixos" "samba"];
    }
    { check = config.services.xserver.displayManager.sddm.enable;
      paths = [
        ["nixos" "sddm" "default"]
        ["nixos" "sddm" "autoLogin"]
      ];
    }
    { check = true;
      path  = ["nixos" "simple"];
    }
    { check = config.services.xserver.displayManager.slim.enable;
      path  = ["nixos" "slim"];
    }
    { check = config.services.snapper.configs != {};
      path  = ["nixos" "snapper"];
    }
    { check = config.services.smokeping.enable;
      path  = ["nixos" "smokeping"];
    }
    { check = config.services.taskserver.enable;
      path  = ["nixos" "taskserver"];
    }
    { check = config.services.tomcat.enable;
      path  = ["nixos" "tomcat"];
    }
    { check = config.services.udisks2.enable;
      path  = ["nixos" "udisks2"];
    }
    { check = config.virtualisation.virtualbox.host.enable;
      paths = [
        ["nixos" "virtualbox" "host-usb-permissions"]
        ["nixos" "virtualbox" "net-hostonlyif"]
        ["nixos" "virtualbox" "simple-cli"]
        ["nixos" "virtualbox" "simple-gui"]
        ["nixos" "virtualbox" "systemd-detect-virt"]
      ];
    }
    { check = config.virtualisation.virtualbox.host.enable
           && config.virtualisation.virtualbox.host.headless;
      path  = ["nixos" "virtualbox" "headless"];
    }
    { check = let
        hasWPSubServiceType = any (y: y.serviceType == "wordpress");
        hasWPSubService = any (x: hasWPSubServiceType x.extraSubservices);
        hasWordPress = config.services.httpd.virtualHosts;
      in config.services.httpd.enable && hasWordPress;
      path  = ["nixos" "wordpress"];
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
