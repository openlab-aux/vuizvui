{ lib, writers, ironbar }:

let
  configFile = let
    sysInfoSpec = [
      { label = "CPU";
        format = [ "{cpu_percent}%" "{temp_c:k10temp-Tctl}°C" ];
      }
      { label = "MEM";
        format = [
          "{memory_used}/{memory_total}GB ({memory_percent}%)"
          "{swap_used}/{swap_total} GB ({swap_percent}%)"
        ];
      }
      { label = "DISK";
        format = "{disk_used#G@/}/{disk_total#G@/}GB"
               + " ({disk_percent:/}%)";
      }
      { label = "NET";
        format = "{net_down:bond0}/{net_up:bond0}Mbps";
      }
      { label = "LAVG";
        format = "{load_average_1}/{load_average_5}/{load_average_15}";
      }
    ];
  in writers.writeJSON "ironbar-config.json" {
    position = "bottom";
    height = 15;
    start = lib.singleton {
      type = "workspaces";
      all_monitors = false;
      sort = "index";
    };
    end = [
      { type = "sys_info";
        format = lib.singleton (lib.concatMapStringsSep " | " (attrs: let
          entries = lib.concatStringsSep " - " (lib.toList attrs.format);
        in "${attrs.label}: ${entries}") sysInfoSpec);
        interval.cpu = 1;
        interval.disks = 300;
        interval.memory = 1;
        interval.networks = 3;
        interval.temps = 5;
      }
      { type = "battery";
        format = "BAT: {percentage}%";
        icon_size = 0;
      }
      { type = "tray";
        icon_size = 15;
        on_click_middle = "sucondary";
      }
      { type = "clock";
        format = "%a %Y-%m-%d %H:%M:%S";
      }
    ];
  };

in (ironbar.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./config.patch ];
  env = (drv.env or {}) // {
    IRONBAR_CONFIG = configFile;
    IRONBAR_CSS = "${./style.css}";
  };
})).override {
  features = [
    "battery"
    "clock"
    "config+json"
    "ipc"
    "label"
    "sys_info"
    "tray"
    "workspaces+niri"
  ];
}
