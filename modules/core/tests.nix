{ options, config, pkgs, lib, ... }:

let
  inherit (lib) any elem;

  isLatestKernel = config.boot.kernelPackages.kernel.version
                == pkgs.linuxPackages_latest.kernel.version;
  wgTestSuffix = "linux-${if isLatestKernel then "latest" else "5_4"}";

  mkTest = attrs: if attrs.check then attrs.paths or [ attrs.path ] else [];

  anyAttrs = pred: cfg: any lib.id (lib.mapAttrsToList (lib.const pred) cfg);
  hasPackage = p: any (x: x.name == p.name) config.environment.systemPackages;

  mkPrometheusExporterTest = name: {
    check = config.services.prometheus.exporters.${name}.enable;
    path = ["nixos" "prometheus-exporters" name];
  };

  upstreamTests = lib.concatMap mkTest [
    { check = config.services._3proxy.enable;
      path  = ["nixos" "_3proxy"];
    }
    { check = config.security.acme.certs != {};
      path  = ["nixos" "acme"];
    }
    { check = config.services.atd.enable;
      path  = ["nixos" "atd"];
    }
    { check = config.services.automysqlbackup.enable;
      path  = ["nixos" "automysqlbackup"];
    }
    { check = config.services.avahi.enable;
      path  = ["nixos" "avahi"];
    }
    { check = config.services.babeld.enable;
      path  = ["nixos" "babeld"];
    }
    { check = elem "bcachefs" config.boot.supportedFilesystems;
      path  = ["nixos" "bcachefs"];
    }
    { check = config.services.beanstalkd.enable;
      path  = ["nixos" "beanstalkd"];
    }
    { check = config.services.beesd.filesystems != {};
      path  = ["nixos" "bees"];
    }
    { check = config.services.bind.enable;
      path  = ["nixos" "bind"];
    }
    { check = config.services.transmission.enable
           || config.services.opentracker.enable;
      path  = ["nixos" "bittorrent"];
    }
    { check = config.services.buildkite-agents != {};
      path  = ["nixos" "buildkite-agents"];
    }
    { check = config.vuizvui.createISO;
      paths = [
        ["nixos" "boot" "biosNetboot"]
        ["nixos" "boot" "uefiNetboot"]
      ];
    }
    { check = config.services.borgbackup.jobs != {}
           || config.services.borgbackup.repos != {};
      path  = ["nixos" "borgbackup"];
    }
    { check = config.services.buildbot-master.enable
           || config.services.buildbot-worker.enable;
      path  = ["nixos" "buildbot"];
    }
    { check = config.services.caddy.enable;
      path  = ["nixos" "caddy"];
    }
    { check = config.services.cadvisor.enable;
      path  = ["nixos" "cadvisor"];
    }
    { check = config.services.cassandra.enable;
      path  = ["nixos" "cassandra"];
    }
    { check = config.services.ceph.enable;
      paths = [
        ["nixos" "ceph-single-node"]
        ["nixos" "ceph-multi-node"]
      ];
    }
    { check = config.services.certmgr.enable;
      path  = ["nixos" "certmgr"];
    }
    { check = config.services.cfssl.enable;
      path  = ["nixos" "cfssl"];
    }
    { check = config.services.cjdns.enable;
      path  = ["nixos" "cjdns"];
    }
    { check = config.services.clickhouse.enable;
      path  = ["nixos" "clickhouse"];
    }
    { check = config.services.cloud-init.enable;
      path  = ["nixos" "cloud-init"];
    }
    { check = config.services.hedgedoc.enable;
      path  = ["nixos" "hedgedoc"];
    }
    { check = config.services.consul.enable;
      path  = ["nixos" "consul"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.hostBridge != null) config.containers;
      path  = ["nixos" "containers-bridge"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.ephemeral) config.containers;
      path  = ["nixos" "containers-ephemeral"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.extraVeths != {}) config.containers;
      path  = ["nixos" "containers-extra_veth"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.localAddress != []) config.containers;
      path  = ["nixos" "containers-hosts"];
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
           && anyAttrs (i: i.forwardPorts != []) config.containers;
      path  = ["nixos" "containers-portforward"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.privateNetwork) config.containers;
      path  = ["nixos" "containers-restart_networking"];
    }
    { check = config.boot.enableContainers
           && anyAttrs (i: i.tmpfs != []) config.containers;
      path  = ["nixos" "containers-tmpfs"];
    }
    { check = config.services.corerad.enable;
      path  = ["nixos" "corerad"];
    }
    { check = config.services.couchdb.enable;
      path  = ["nixos" "couchdb"];
    }
    { check = config.services.deluge.enable;
      path  = ["nixos" "deluge"];
    }
    { check = config.security.dhparams.enable
           && config.security.dhparams.stateful;
      path  = ["nixos" "dhparams"];
    }
    { check = config.services.dnscrypt-proxy2.enable;
      path  = ["nixos" "dnscrypt-proxy2"];
    }
    { check = config.virtualisation.docker.enable;
      path  = ["nixos" "docker-tools"];
    }
    { check = config.virtualisation.oci-containers.containers != {};
      path  = ["nixos" "oci-containers"];
    }
    { check = with config.virtualisation.docker; enable
           && package.name == pkgs.docker-edge.name;
      path  = ["nixos" "docker-edge"];
    }
    { check = config.services.dockerRegistry.enable;
      path  = ["nixos" "docker-registry"];
    }
    { check = config.virtualisation.docker.enable
           && config.virtualisation.docker.storageDriver == "overlay";
      path  = ["nixos" "docker-tools-overlay"];
    }
    { check = config.services.documize.enable;
      path  = ["nixos" "documize"];
    }
    { check = config.services.dokuwiki != {};
      path  = ["nixos" "dokuwiki"];
    }
    { check = config.services.dovecot2.enable;
      path  = ["nixos" "dovecot"];
    }
    { check = config.services.ejabberd.enable;
      path  = ["nixos" "ejabberd"];
    }
    { check = config.services.logstash.enable
           || config.services.elasticsearch.enable
           || config.services.kibana.enable;
      path  = ["nixos" "elk"];
    }
    { check = config.services.etcd.enable;
      paths = [
        ["nixos" "etcd"]
        ["nixos" "etcd-cluster"]
      ];
    }
    { check = config.hardware.fancontrol.enable;
      path  = ["nixos" "fancontrol"];
    }
    { check = config.services.ferm.enable;
      path  = ["nixos" "ferm"];
    }
    { check = config.programs.fish.enable;
      path  = ["nixos" "fish"];
    }
    { check = config.services.flannel.enable;
      path  = ["nixos" "flannel"];
    }
    { check = config.services.fluentd.enable;
      path  = ["nixos" "fluentd"];
    }
    { check = config.services.freeswitch.enable;
      path  = ["nixos" "freeswitch"];
    }
    { check = true;
      path  = ["nixos" "fsck"];
    }
    { check = config.services.gotify.enable;
      path  = ["nixos" "gotify-server"];
    }
    { check = config.services.grocy.enable;
      path  = ["nixos" "grocy"];
    }
    { check = config.services.gitea.enable;
      path  = ["nixos" "gitea"];
    }
    { check = config.services.gitlab.enable;
      path  = ["nixos" "gitlab"];
    }
    { check = config.services.gitolite.enable;
      path  = ["nixos" "gitolite"];
    }
    { check = config.services.gitolite.enable
           && config.services.fcgiwrap.enable;
      path  = ["nixos" "gitolite-fcgiwrap"];
    }
    { check = config.services.glusterfs.enable;
      path  = ["nixos" "glusterfs"];
    }
    { check = config.services.xserver.desktopManager.gnome.enable;
      path  = ["nixos" "installed-tests"];
    }
    { check = config.services.gocd-agent.enable;
      path  = ["nixos" "gocd-agent"];
    }
    { check = config.services.gocd-server.enable;
      path  = ["nixos" "gocd-server"];
    }
    { check = config.security.googleOsLogin.enable;
      path  = ["nixos" "google-oslogin"];
    }
    { check = config.services.grafana.enable;
      path  = ["nixos" "grafana"];
    }
    { check = with config.services.graphite; carbon.enableCache
           || carbon.enableAggregator || carbon.enableRelay
           || web.enable || api.enable || seyren.enable
           || beacon.enable;
      path  = ["nixos" "graphite"];
    }
    { check = config.services.graylog.enable;
      path  = ["nixos" "graylog"];
    }
    { check = hasPackage pkgs.gvisor;
      path  = ["nixos" "gvisor"];
    }
    { check = config.services.hadoop.hdfs.namenode.enabled
           || config.services.hadoop.hdfs.datanode.enabled;
      path  = ["nixos" "hadoop" "hdfs"];
    }
    { check = config.services.hadoop.yarn.resourcemanager.enabled
           || config.services.hadoop.yarn.nodemanager.enabled;
      path  = ["nixos" "hadoop" "yarn"];
    }
    { check = hasPackage pkgs.handbrake;
      path  = ["nixos" "handbrake"];
    }
    { check = config.services.haproxy.enable;
      path  = ["nixos" "haproxy"];
    }
    { check = config.services.hitch.enable;
      path  = ["nixos" "hitch"];
    }
    { check = config.services.home-assistant.enable;
      path  = ["nixos" "home-assistant"];
    }
    { check = config.services.hound.enable;
      path  = ["nixos" "hound"];
    }
    { check = config.services.hydra.enable;
      path  = ["nixos" "hydra"];
    }
    { check = config.services.icingaweb2.enable;
      path  = ["nixos" "icingaweb2"];
    }
    { check = config.programs.iftop.enable;
      path  = ["nixos" "iftop"];
    }
    { check = config.services.ihatemoney.enable;
      path  = ["nixos" "ihatemoney"];
    }
    { check = config.services.incron.enable;
      path  = ["nixos" "incron"];
    }
    { check = config.services.influxdb.enable;
      path  = ["nixos" "influxdb"];
    }
    { check = config.boot.initrd.network.enable;
      path  = ["nixos" "initrdNetwork"];
    }
    { check = config.boot.initrd.network.ssh.enable;
      path  = ["nixos" "initrd-network-ssh"];
    }
    { check = anyAttrs (f: f.encrypted.enable) config.fileSystems
           || lib.any (s: s.encrypted.enable) config.swapDevices;
      path  = ["nixos" "installer" "encryptedFSWithKeyfile"];
    }
    { check = config.boot.loader.grub.version == 1;
      path  = ["nixos" "installer" "grub1"];
    }
    { check = config.boot.initrd.luks.devices != {};
      paths = [
        ["nixos" "installer" "luksroot-format1"]
        ["nixos" "installer" "luksroot-format2"]
      ];
    }
    { check = elem "ext3" config.boot.supportedFilesystems
           && config.specialisation != {};
      path  = ["nixos" "installer" "simpleClone"];
    }
    { check = config.boot.loader.grub.device == "nodev"
           && config.boot.loader.grub.efiSupport;
      path  = ["nixos" "installer" "simpleUefiGrub"];
    }
    { check = config.boot.loader.grub.device == "nodev"
           && config.boot.loader.grub.efiSupport
           && config.specialisation != {};
      path  = ["nixos" "installer" "simpleUefiGrubClone"];
    }
    { check = elem "zfs" config.boot.supportedFilesystems;
      path  = ["nixos" "installer" "zfsroot"];
    }
    { check = config.services.jackett.enable;
      path  = ["nixos" "jackett"];
    }
    { check = config.services.jellyfin.enable;
      path  = ["nixos" "jellyfin"];
    }
    { check = config.services.jenkins.enable;
      path  = ["nixos" "jenkins"];
    }
    { check = config.services.apache-kafka.enable;
      path  = ["nixos" "kafka"];
    }
    { check = config.services.keepalived.enable;
      path  = ["nixos" "keepalived"];
    }
    { check = let
        isHeimdal = lib.hasPrefix "heimdal" config.krb5.kerberos.name;
        isServer = config.services.kerberos_server.enable;
      in isHeimdal && (isServer || config.krb5.enable);
      path  = ["nixos" "kerberos" "heimdal"];
    }
    { check = let
        isHeimdal = lib.hasPrefix "heimdal" config.krb5.kerberos.name;
        isServer = config.services.kerberos_server.enable;
      in !isHeimdal && (isServer || config.krb5.enable);
      path  = ["nixos" "kerberos" "mit"];
    }
    (let
      kernelVersion = config.boot.kernelPackages.kernel.version;
      majorMinor = lib.concatStringsSep "_"
        (lib.take 2 (builtins.splitVersion kernelVersion));
      kernelTestName = "linux_${majorMinor}";
    in {
      # isDerivation is a workaround to fix eval until
      # https://github.com/NixOS/nixpkgs/pull/119307 lands.
      check = lib.isDerivation
        (pkgs.nixosTests.kernel-generic."${kernelTestName}" or null);
      path  = ["nixos" "kernel-generic" kernelTestName ];
    })
    { check = config.services.knot.enable;
      path  = ["nixos" "knot"];
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
    { check = config.services.openldap.enable
           || config.users.ldap.enable;
      path  = ["nixos" "ldap"];
    }
    { check = config.services.leaps.enable;
      path  = ["nixos" "leaps"];
    }
    { check = config.services.lidarr.enable;
      path  = ["nixos" "lidarr"];
    }
    { check = config.services.limesurvey.enable;
      path  = ["nixos" "limesurvey"];
    }
    { check = config.services.loki.enable;
      path  = ["nixos" "loki"];
    }
    { check = hasPackage pkgs.lorri;
      path  = ["nixos" "lorri"];
    }
    { check = config.services.magnetico.enable;
      path  = ["nixos" "magnetico"];
    }
    { check = config.services.mailcatcher.enable;
      path  = ["nixos" "mailcatcher"];
    }
    { check = config.services.matomo.enable;
      path  = ["nixos" "matomo"];
    }
    { check = config.services.matrix-synapse.enable;
      path  = ["nixos" "matrix-synapse"];
    }
    { check = config.services.mediawiki.enable;
      path  = ["nixos" "mediawiki"];
    }
    { check = config.services.memcached.enable;
      path  = ["nixos" "memcached"];
    }
    { check = config.services.metabase.enable;
      path  = ["nixos" "metabase"];
    }
    { check = config.services.miniflux.enable;
      path  = ["nixos" "miniflux"];
    }
    { check = config.services.minio.enable;
      path  = ["nixos" "minio"];
    }
    { check = config.services.minidlna.enable;
      path  = ["nixos" "minidlna"];
    }
    { check = config.services.moinmoin.enable;
      path  = ["nixos" "moinmoin"];
    }
    { check = config.services.mongodb.enable;
      path  = ["nixos" "mongodb"];
    }
    { check = config.services.moodle.enable;
      path  = ["nixos" "moodle"];
    }
    { check = config.services.morty.enable;
      path  = ["nixos" "morty"];
    }
    { check = config.services.mosquitto.enable;
      path  = ["nixos" "mosquitto"];
    }
    { check = config.services.mpd.enable;
      path  = ["nixos" "mpd"];
    }
    { check = config.services.murmur.enable;
      path  = ["nixos" "mumble"];
    }
    { check = config.services.munin-node.enable
           || config.services.munin-cron.enable;
      path  = ["nixos" "munin"];
    }
    { check = config.services.mxisd.enable;
      path  = ["nixos" "mxisd"];
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
    { check = config.services.nagios.enable;
      path  = ["nixos" "nagios"];
    }
    { check = config.services.ndppd.enable;
      path  = ["nixos" "ndppd"];
    }
    { check = config.services.neo4j.enable;
      path  = ["nixos" "neo4j"];
    }
    { check = config.specialisation != {};
      path  = ["nixos" "specialisation"];
    }
    { check = config.services.netdata.enable;
      path  = ["nixos" "netdata"];
    }
    { check = with config.networking.proxy; any (val: val != null)
            [ default allProxy ftpProxy httpProxy httpsProxy noProxy
              rsyncProxy
            ];
      path  = ["nixos" "networkingProxy"];
    }
    { check = config.services.nextcloud.enable;
      path  = ["nixos" "nextcloud" "basic"];
    }
    { check = config.services.nextcloud.enable
           && config.services.nextcloud.config.dbtype == "mysql";
      path  = ["nixos" "nextcloud" "with-mysql-and-memcached"];
    }
    { check = config.services.nextcloud.enable
           && config.services.nextcloud.config.dbtype == "pgsql";
      path  = ["nixos" "nextcloud" "with-postgresql-and-redis"];
    }
    { check = config.services.nexus.enable;
      path  = ["nixos" "nexus"];
    }
    { check = config.services.nghttpx.enable;
      path  = ["nixos" "nghttpx"];
    }
    { check = config.services.nginx.enable;
      paths = [
        ["nixos" "nginx"]
        ["nixos" "nginx-etag"]
      ];
    }
    { check = config.services.nginx.sso.enable;
      path  = ["nixos" "nginx-sso"];
    }
    { check = config.nix.sshServe.enable;
      path  = ["nixos" "nix-ssh-serve"];
    }
    { check = true;
      path  = ["nixos" "nixos-generate-config"];
    }
    { check = config.services.novacomd.enable;
      path  = ["nixos" "novacomd"];
    }
    { check = config.services.nsd.enable;
      path  = ["nixos" "nsd"];
    }
    { check = config.services.nzbget.enable;
      path  = ["nixos" "nzbget"];
    }
    { check = config.services.openarena.enable;
      path  = ["nixos" "openarena"];
    }
    { check = config.services.openldap.enable;
      path  = ["nixos" "openldap"];
    }
    { check = config.services.opensmtpd.enable;
      path  = ["nixos" "opensmtpd"];
    }
    { check = config.services.orangefs.client.enable
           || config.services.orangefs.server.enable;
      path  = ["nixos" "orangefs"];
    }
    { check = config.boot.loader.grub.enable
           && config.boot.loader.grub.useOSProber;
      path  = ["nixos" "os-prober"];
    }
    { check = config.services.osrm.enable;
      path  = ["nixos" "osrm-backend"];
    }
    { check = true;
      path  = ["nixos" "overlayfs"];
    }
    { check = config.services.packagekit.enable;
      path  = ["nixos" "packagekit"];
    }
    { check = config.security.pam.oath.enable;
      path  = ["nixos" "pam-oath-login"];
    }
    { check = config.security.pam.u2f.enable;
      path  = ["nixos" "pam-u2f"];
    }
    { check = config.services.paperless-ng.enable;
      path  = ["nixos" "paperless-ng"];
    }
    { check = config.services.peerflix.enable;
      path  = ["nixos" "peerflix"];
    }
    { check = with config.services.postgresql; enable
           && lib.any (lib.hasPrefix "pgjwt") extraPlugins;
      path  = ["nixos" "pgjwt"];
    }
    { check = config.services.pgmanage.enable;
      path  = ["nixos" "pgmanage"];
    }
    { check = config.programs.plotinus.enable;
      path  = ["nixos" "plotinus"];
    }
    { check = with config.services.postgresql; enable
           && lib.any (lib.hasPrefix "postgis") extraPlugins;
      path  = ["nixos" "postgis"];
    }
    { check = config.services.postgresql.enable;
      path  = let
        filterPg = name: drv: lib.hasPrefix "postgresql" name
                           && drv == config.services.postgresql.package;
        pgPackage = lib.head (lib.attrNames (lib.filterAttrs filterPg pkgs));
      in ["nixos" "postgresql" pgPackage];
    }
    { check = config.services.postgresqlWalReceiver.receivers != {};
      path  = ["nixos" "postgresql-wal-receiver"];
    }
    { check = config.services.powerdns.enable;
      path  = ["nixos" "powerdns"];
    }
    { check = config.services.pppd.enable;
      path  = ["nixos" "pppd"];
    }
    { check = config.services.prometheus.enable;
      path  = ["nixos" "prometheus"];
    }
    # TODO: Generate automatically!
    (mkPrometheusExporterTest "bind")
    (mkPrometheusExporterTest "blackbox")
    (mkPrometheusExporterTest "collectd")
    (mkPrometheusExporterTest "dnsmasq")
    (mkPrometheusExporterTest "dovecot")
    (mkPrometheusExporterTest "fritzbox")
    (mkPrometheusExporterTest "json")
    (mkPrometheusExporterTest "mail")
    (mkPrometheusExporterTest "nextcloud")
    (mkPrometheusExporterTest "nginx")
    (mkPrometheusExporterTest "node")
    (mkPrometheusExporterTest "postfix")
    (mkPrometheusExporterTest "postgres")
    (mkPrometheusExporterTest "rspamd")
    (mkPrometheusExporterTest "snmp")
    (mkPrometheusExporterTest "surfboard")
    (mkPrometheusExporterTest "tor")
    (mkPrometheusExporterTest "varnish")
    (mkPrometheusExporterTest "wireguard")
    { check = config.services.prosody.enable;
      path  = ["nixos" "prosody"];
    }
    { check = with config.services.prosody; enable
           && builtins.match ".*MySQL.*" extraConfig != null;
      path  = ["nixos" "prosodyMysql"];
    }
    { check = config.services.rabbitmq.enable;
      path  = ["nixos" "rabbitmq"];
    }
    { check = config.services.radarr.enable;
      path  = ["nixos" "radarr"];
    }
    { check = config.services.radicale.enable;
      path  = ["nixos" "radicale"];
    }
    { check = config.services.redis.enable;
      path  = ["nixos" "redis"];
    }
    { check = config.services.redmine.enable;
      path  = ["nixos" "redmine"];
    }
    { check = config.services.restic.backups != {};
      path  = ["nixos" "restic"];
    }
    { check = config.services.roundcube.enable;
      path  = ["nixos" "roundcube"];
    }
    { check = config.services.rspamd.enable;
      path  = ["nixos" "rspamd"];
    }
    { check = config.services.rss2email.enable;
      path  = ["nixos" "rss2email"];
    }
    { check = config.services.rsyslogd.enable;
      path  = ["nixos" "rsyslogd"];
    }
    { check = config.networking.rxe.enable;
      path  = ["nixos" "rxe"];
    }
    { check = config.services.samba.enable;
      path  = ["nixos" "samba"];
    }
    { check = config.services.sanoid.enable;
      path  = ["nixos" "sanoid"];
    }
    { check = config.services.xserver.displayManager.sddm.enable
           && config.services.xserver.displayManager.autoLogin.enable;
      paths = ["nixos" "sddm" "autoLogin"];
    }
    { check = config.services.shiori.enable;
      path  = ["nixos" "shiori"];
    }
    { check = hasPackage pkgs.signal-desktop;
      path  = ["nixos" "signal-desktop"];
    }
    { check = config.services.slurm.enableStools
           || config.services.slurm.client.enable
           || config.services.slurm.server.enable
           || config.services.slurm.dbdserver.enable;
      path = ["nixos" "slurm"];
    }
    { check = config.services.smokeping.enable;
      path  = ["nixos" "smokeping"];
    }
    { check = config.services.snapper.configs != {};
      path  = ["nixos" "snapper"];
    }
    { check = config.services.solr.enable;
      path  = ["nixos" "solr"];
    }
    { check = config.services.spacecookie.enable;
      path  = ["nixos" "spacecookie"];
    }
    { check = config.services.sonarr.enable;
      path  = ["nixos" "sonarr"];
    }
    { check = config.services.strongswan-swanctl.enable;
      path  = ["nixos" "strongswan-swanctl"];
    }
    { check = config.security.sudo.enable;
      path  = ["nixos" "sudo"];
    }
    { check = config.services.sympa.enable;
      path  = ["nixos" "sympa"];
    }
    { check = config.services.syncthing.enable;
      path  = ["nixos" "syncthing-init"];
    }
    { check = config.services.syncthing.relay.enable;
      path  = ["nixos" "syncthing-relay"];
    }
    { check = true;
      paths = [
        ["nixos" "systemd"]
        ["nixos" "systemd-analyze"]
        ["nixos" "systemd-nspawn"]
        ["nixos" "systemd-timesyncd"]
      ];
    }
    { check = anyAttrs (s: s.confinement.enable) config.systemd.services;
      path  = ["nixos" "systemd-confinement"];
    }
    { check = let
        isVrf = anyAttrs (n: n.netdevConfig.Kind or "" == "vrf");
      in config.networking.useNetworkd && isVrf config.systemd.network.netdevs;
      path  = ["nixos" "systemd-networkd-vrf"];
    }
    { check = let
        isWG = anyAttrs (n: n.netdevConfig.Kind or "" == "wireguard");
      in config.networking.useNetworkd && isWG config.systemd.network.netdevs;
      path  = ["nixos" "systemd-networkd-wireguard"];
    }
    { check = config.services.pdns-recursor.enable;
      path  = ["nixos" "pdns-recursor"];
    }
    { check = config.services.taskserver.enable;
      path  = ["nixos" "taskserver"];
    }
    { check = config.services.telegraf.enable;
      path  = ["nixos" "telegraf"];
    }
    { check = config.services.tiddlywiki.enable;
      path  = ["nixos" "tiddlywiki"];
    }
    { check = true;
      path  = ["nixos" "timezone"];
    }
    { check = config.services.tinydns.enable;
      path  = ["nixos" "tinydns"];
    }
    { check = config.services.tor.enable;
      path  = ["nixos" "tor"];
    }
    { check = config.services.transmission.enable;
      path  = ["nixos" "transmission"];
    }
    { check = config.services.trac.enable;
      path  = ["nixos" "trac"];
    }
    { check = config.services.trilium-server.enable;
      path  = ["nixos" "trilium-server"];
    }
    { check = config.services.trezord.enable;
      path  = ["nixos" "trezord"];
    }
    { check = config.services.trickster.enable;
      path  = ["nixos" "trickster"];
    }
    { check = config.services.miniupnpd.enable
           || hasPackage pkgs.miniupnpc_2;
      path  = ["nixos" "upnp"];
    }
    { check = config.services.uwsgi.enable;
      path  = ["nixos" "uwsgi"];
    }
    { check = config.services.vault.enable;
      path  = ["nixos" "vault"];
    }
    { check = config.services.victoriametrics.enable;
      path  = ["nixos" "victoriametrics"];
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
    { check = config.networking.wireguard.enable;
      path  = ["nixos" "wireguard" "wireguard-basic-${wgTestSuffix}"];
    }
    { check = with config.networking.wireguard; enable
           && anyAttrs (i: i.generatePrivateKeyFile) interfaces;
      path  = ["nixos" "wireguard" "wireguard-generated-${wgTestSuffix}"];
    }
    { check = let
        isEnabled = config.networking.wireguard.enable;
        usesNS = iface: iface.socketNamespace != null
              || iface.interfaceNamespace != null;
      in isEnabled && anyAttrs usesNS config.networking.wireguard.interfaces;
      path  = ["nixos" "wireguard" "wireguard-namespaces-${wgTestSuffix}"];
    }
    { check = config.networking.wg-quick.interfaces != {};
      path  = ["nixos" "wireguard" "wireguard-wg-quick-${wgTestSuffix}"];
    }
    { check = config.services.wordpress != {};
      path  = ["nixos" "wordpress"];
    }
    { check = config.services.xandikos.enable;
      path  = ["nixos" "xandikos"];
    }
    { check = config.services.xserver.xautolock.enable;
      path  = ["nixos" "xautolock"];
    }
    { check = config.services.xserver.windowManager.xmonad.enable;
      path  = ["nixos" "xmonad"];
    }
    { check = config.services.xrdp.enable;
      path  = ["nixos" "xrdp"];
    }
    { check = config.programs.xss-lock.enable;
      path  = ["nixos" "xss-lock"];
    }
    { check = config.programs.yabar.enable;
      path  = ["nixos" "yabar"];
    }
    { check = config.services.yggdrasil.enable;
      path  = ["nixos" "yggdrasil"];
    }
    { check = elem "zfs" config.boot.supportedFilesystems
           && !config.boot.zfs.enableUnstable;
      path  = ["nixos" "zfs" "stable"];
    }
    { check = elem "zfs" config.boot.supportedFilesystems
           && config.boot.zfs.enableUnstable;
      path  = ["nixos" "zfs" "unstable"];
    }
    { check = config.programs.zsh.enable;
      path  = ["nixos" "zsh-history"];
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
