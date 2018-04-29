{ options, config, pkgs, lib, ... }:

let
  inherit (lib) any elem;

  whichNet = if config.networking.useNetworkd then "networkd" else "scripted";

  mkTest = attrs: if attrs.check then attrs.paths or [ attrs.path ] else [];

  anyAttrs = pred: cfg: any lib.id (lib.mapAttrsToList (lib.const pred) cfg);
  hasPackage = p: any (x: x.name == p.name) config.environment.systemPackages;

  upstreamTests = lib.concatMap mkTest [
    { check = config.security.acme.certs != {};
      path  = ["nixos" "acme"];
    }
    { check = config.services.atd.enable;
      path  = ["nixos" "atd"];
    }
    { check = config.services.avahi.enable;
      path  = ["nixos" "avahi"];
    }
    { check = config.services.beegfsEnable;
      path  = ["nixos" "beegfs"];
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
    { check = config.services.buildbot-master.enable
           || config.services.buildbot-worker.enable;
      path  = ["nixos" "buildbot"];
    }
    { check = config.services.cadvisor.enable;
      path  = ["nixos" "cadvisor"];
    }
    { check = config.services.ceph.enable
           || config.services.ceph.mon.enable
           || config.services.ceph.mgr.enable
           || config.services.ceph.osd.enable;
      path  = ["nixos" "ceph"];
    }
    { check = hasPackage pkgs.chromium;
      path  = ["nixos" "chromium"];
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
    { check = config.services.couchdb.enable;
      path  = ["nixos" "couchdb"];
    }
    { check = config.services.deluge.enable;
      path  = ["nixos" "deluge"];
    }
    { check = config.services.dnscrypt-proxy.enable;
      path  = ["nixos" "dnscrypt-proxy"];
    }
    { check = config.virtualisation.docker.enable;
      paths = [
        ["nixos" "docker"]
        ["nixos" "docker-tools"]
      ];
    }
    { check = with config.virtualisation.docker; enable
           && package.name == pkgs.docker-edge.name;
      path  = ["nixos" "docker-edge"];
    }
    { check = config.virtualisation.docker.enable
           && config.virtualisation.docker.storageDriver == "overlay";
      path  = ["nixos" "docker-tools-overlay"];
    }
    { check = config.services.dovecot2.enable;
      path  = ["nixos" "dovecot"];
    }
    { check = config.security.pam.enableEcryptfs;
      path  = ["nixos" "ecryptfs"];
    }
    { check = true;
      path  = ["nixos" "env"];
    }
    { check = config.services.etcd.enable;
      path  = ["nixos" "etcd"];
    }
    { check = config.services.ferm.enable;
      path  = ["nixos" "ferm"];
    }
    { check = hasPackage pkgs.firefox;
      path  = ["nixos" "firefox"];
    }
    { check = config.networking.firewall.enable;
      path  = ["nixos" "firewall"];
    }
    { check = config.services.fleet.enable;
      path  = ["nixos" "fleet"];
    }
    { check = config.services.fwupd.enable;
      path  = ["nixos" "fwupd"];
    }
    { check = config.services.gitolite.enable;
      path  = ["nixos" "gitolite"];
    }
    { check = hasPackage pkgs.gnome-desktop-testing;
      path  = ["nixos" "gjs"];
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
    { check = config.services.grafana.enable;
      path  = ["nixos" "grafana"];
    }
    { check = with config.services.graphite; carbon.enableCache
           || carbon.enableAggregator || carbon.enableRelay
           || web.enable || api.enable || seyren.enable || pager.enable
           || beacon.enable;
      path  = ["nixos" "graphite"];
    }
    { check = config.security.lockKernelModules
           || config.security.hideProcessInformation
           || config.boot.kernel.sysctl."user.max_user_namespaces" or 1 == 0;
      path  = ["nixos" "hardened"];
    }
    { check = true;
      path  = ["nixos" "hibernate"];
    }
    { check = config.services.home-assistant.enable;
      path  = ["nixos" "home-assistant"];
    }
    { check = config.services.hound.enable;
      path  = ["nixos" "hound"];
    }
    { check = config.services.xserver.windowManager.i3.enable;
      path  = ["nixos" "i3wm"];
    }
    { check = config.programs.iftop.enable;
      path  = ["nixos" "iftop"];
    }
    { check = config.boot.initrd.network.enable;
      path  = ["nixos" "initrdNetwork"];
    }
    { check = config.boot.initrd.network.ssh.enable;
      path  = ["nixos" "initrd-network-ssh"];
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
    { check = elem config.boot.kernelPackages.kernel.version
            [ pkgs.linuxPackages_copperhead_lts.kernel.version
              pkgs.linuxPackages_copperhead_stable.kernel.version
            ];
      path  = ["nixos" "kernel-copperhead"];
    }
    { check = config.boot.kernelPackages.kernel.version
           == pkgs.linuxPackages_latest.kernel.version;
      path  = ["nixos" "kernel-latest"];
    }
    { check = config.boot.kernelPackages.kernel.version
           == pkgs.linuxPackages.kernel.version;
      path  = ["nixos" "kernel-lts"];
    }
    { check = with config.services.kubernetes; apiserver.enable
           || scheduler.enable || controllerManager.enable || kubelet.enable
           || proxy.enable;
      paths = [
        ["nixos" "kubernetes" "dns" "singlenode"]
        ["nixos" "kubernetes" "dns" "multinode"]
        ["nixos" "kubernetes" "rbac" "singlenode"]
        ["nixos" "kubernetes" "rbac" "multinode"]
      ];
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
    { check = config.services.matrix-synapse.enable;
      path  = ["nixos" "matrix-synapse"];
    }
    { check = config.services.mesos.master.enable
           || config.services.mesos.slave.enable;
      path  = ["nixos" "mesos"];
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
    { check = true;
      path  = ["nixos" "mutableUsers"];
    }
    { check = config.services.mysql.enable;
      path  = ["nixos" "mysql"];
    }
    { check = config.services.mysqlBackup.enable;
      path  = ["nixos" "mysqlBackup"];
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
    { check = config.services.netdata.enable;
      path  = ["nixos" "netdata"];
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
    { check = anyAttrs (i: i.ipv4.addresses != [])
              config.networking.interfaces;
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
    { check = config.services.nghttpx.enable;
      path  = ["nixos" "nghttpx"];
    }
    { check = config.services.nginx.enable;
      path  = ["nixos" "nginx"];
    }
    { check = config.nix.sshServe.enable;
      path  = ["nixos" "nix-ssh-serve"];
    }
    { check = config.services.novacomd.enable;
      path  = ["nixos" "novacomd"];
    }
    { check = config.services.nsd.enable;
      path  = ["nixos" "nsd"];
    }
    { check = config.services.openssh.enable;
      path  = ["nixos" "openssh"];
    }
    { check = config.services.openldap.enable;
      path  = ["nixos" "openldap"];
    }
    { check = let
        hasOCSubServiceType = any (y: y.serviceType == "owncloud");
        hasOCSubService = any (x: hasOCSubServiceType x.extraSubservices);
        hasOwnCloud = config.services.httpd.virtualHosts;
      in config.services.httpd.enable && hasOwnCloud;
      path  = ["nixos" "owncloud"];
    }
    { check = config.security.pam.oath.enable;
      path  = ["nixos" "pam-oath-login"];
    }
    { check = config.services.peerflix.enable;
      path  = ["nixos" "peerflix"];
    }
    { check = config.services.xserver.desktopManager.plasma5.enable;
      path  = ["nixos" "plasma5"];
    }
    { check = config.programs.plotinus.enable;
      path  = ["nixos" "plotinus"];
    }
    { check = config.services.postgresql.enable;
      path  = let
        filterPg = name: drv: lib.hasPrefix "postgresql" name
                           && drv == config.services.postgresql.package;
        pgPackage = lib.head (lib.attrNames (lib.filterAttrs filterPg pkgs));
      in ["nixos" "postgresql" pgPackage];
    }
    { check = config.services.powerdns.enable;
      path  = ["nixos" "powerdns"];
    }
    { check = config.networking.usePredictableInterfaceNames;
      path  = ["nixos" "predictable-interface-names"];
    }
    { check = config.services.printing.enable;
      path  = ["nixos" "printing"];
    }
    { check = config.services.prometheus.enable;
      path  = ["nixos" "prometheus"];
    }
    { check = config.services.prosody.enable;
      path  = ["nixos" "prosody"];
    }
    { check = config.services.httpd.enable
           && elem "proxy_balancer" config.services.httpd.extraModules;
      path  = ["nixos" "proxy"];
    }
    { check = config.services.quagga.ospf.enable;
      path  = ["nixos" "quagga"];
    }
    { check = config.hardware.opengl.driSupport
           && config.services.xserver.enable;
      path  = ["nixos" "quake3"];
    }
    { check = config.services.rabbitmq.enable;
      path  = ["nixos" "rabbitmq"];
    }
    { check = config.services.radicale.enable;
      path  = ["nixos" "radicale"];
    }
    { check = config.services.rspamd.enable;
      path  = ["nixos" "rspamd"];
    }
    { check = true;
      path  = ["nixos" "runInMachine"];
    }
    { check = config.networking.rxe.enable;
      path  = ["nixos" "rxe"];
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
    { check = config.services.statsd.enable;
      path  = ["nixos" "statsd"];
    }
    { check = config.services.strongswan-swanctl.enable;
      path  = ["nixos" "strongswan-swanctl"];
    }
    { check = config.security.sudo.enable;
      path  = ["nixos" "sudo"];
    }
    { check = true;
      path  = ["nixos" "switchTest"];
    }
    { check = true;
      path  = ["nixos" "systemd"];
    }
    { check = config.services.taskserver.enable;
      path  = ["nixos" "taskserver"];
    }
    { check = config.services.tomcat.enable;
      path  = ["nixos" "tomcat"];
    }
    { check = config.services.transmission.enable;
      path  = ["nixos" "transmission"];
    }
    { check = config.services.udisks2.enable;
      path  = ["nixos" "udisks2"];
    }
    { check = config.services.vault.enable;
      path  = ["nixos" "vault"];
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
    { check = config.services.xserver.xautolock.enable;
      path  = ["nixos" "xautolock"];
    }
    { check = config.services.xserver.desktopManager.xfce.enable;
      path  = ["nixos" "xfce"];
    }
    { check = config.services.xserver.windowManager.xmonad.enable;
      path  = ["nixos" "xmonad"];
    }
    { check = config.services.xrdp.enable;
      path  = ["nixos" "xrdp"];
    }
    { check = config.programs.yabar.enable;
      path  = ["nixos" "yabar"];
    }
    { check = config.services.zookeeper.enable;
      path  = ["nixos" "zookeeper"];
    }
  ];

in {
  options.vuizvui = {
    requiresTests = lib.mkOption {
      type = lib.types.listOf (lib.types.listOf lib.types.str);
      default = [];
      example = [ ["nixos" "nat" "firewall"] ["vuizvui" "foo"] ];
      description = ''
        A list of attribute paths to the tests which need to succeed in order
        to trigger a channel update for the current configuration/machine.

        Every attribute path itself is a list of attribute names, which are
        queried using <function>lib.getAttrFromPath</function>.
      '';
    };
  };

  config.vuizvui.requiresTests = upstreamTests;
}
