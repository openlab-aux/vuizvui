{ pkgs ? import (import ../../../../../nixpkgs-path.nix) {}
, lib ? import "${import ../../../../../nixpkgs-path.nix}/lib"
, timeout ? 300
}:

with lib;

let
  baseConfig = pkgs.writeText "conkyrc" ''
    conky.config = {
      cpu_avg_samples = 2,
      net_avg_samples = 2,
      no_buffers = true,
      out_to_console = true,
      out_to_ncurses = false,
      out_to_stderr = false,
      out_to_x = false,
      extra_newline = false,
      update_interval = 1.0,
      uppercase = false,
      use_spacer = 'none',
      pad_percents = 3,
      use_spacer = 'left',
    };

    conky.text = ''';
  '';

  optexpr = name: expr: "\${${name}_disabled:-\\\${${name} ${expr}\\}}";
  cexpr = name: args: "${optexpr name (concatStringsSep " " args)}";

  mkNetInfo = iface: let
    upspeed = cexpr "upspeed" [ iface ];
    downspeed = cexpr "downspeed" [ iface ];
  in "${upspeed} ${downspeed}";

  mkDiskFree = path: let
    used = cexpr "fs_used" [ path ];
    size = cexpr "fs_size" [ path ];
  in "${used}/${size}";

  gpuTemp = "${cexpr "hwmon" [ "0" "temp" "1" ]}C";

  weather = (cexpr "weather" [
    "http://tgftp.nws.noaa.gov/data/observations/metar/stations/"
    "EDMA"
    "temperature"
  ]) + "C";

  mkConky = args: let
    time = cexpr "time" [ "%a %b %d %T %Z %Y" ];
    text = concatStringsSep " | " (args ++ singleton time);
  in pkgs.writeScript "conky-run.sh" ''
    #!${pkgs.stdenv.shell}
    PATH="${pkgs.coreutils}/bin"

    cpuload() {
      for i in $(seq 1 $(nproc))
      do
        [ $i -eq 1 ] || echo -n ' '
        echo -n "\''${cpu cpu$i}%"
      done
    }

    cputemp_collect() {
      for i in /sys/bus/platform/devices/coretemp.?/hwmon/hwmon?/temp?_input
      do
        [ -e "$i" ] || continue
        echo "$i" | ${pkgs.gnused}/bin/sed -re \
          's/^.*hwmon([0-9]+)[^0-9]*([0-9]+).*$/''${hwmon \1 temp \2}/'
      done
    }

    cputemp() {
      echo $(cputemp_collect)
    }

    tries=0
    while ! raw_netinfo="$(${
      "${pkgs.iproute2}/sbin/ip route get 8.8.8.8 2> /dev/null"
    })"; do
      if [ $tries -ge ${toString timeout} ]; then
        upspeed_disabled=N/A
        downspeed_disabled=N/A
        break
      fi
      echo "Waiting for primary network interface to become available..."
      tries=$(($tries + 1))
      sleep 1
    done

    primary_netdev="$(echo "$raw_netinfo" | \
      ${pkgs.gnused}/bin/sed -nre 's/^.*dev *([^ ]+).*$/\1/p')"

    # FIXME: Log stderr to the journal!
    ${pkgs.conky}/bin/conky -c "${baseConfig}" -t "${text}" 2> /dev/null
  '';

in {
  left = mkConky [
    "CPU: $(cpuload) - ${cexpr "cpu" [ "cpu0" ]}%"
    "MEM: \\$mem/\\$memmax - \\$memperc%"
    "SWAP: \\$swap/\\$swapmax \\$swapperc%"
  ];

  right = mkConky [
    "NET: ${mkNetInfo "$primary_netdev"}"
    "DF: ${mkDiskFree "/"}"
    "LAVG: \\$loadavg"
    "TEMP - CPU: $(cputemp) - GPU: ${gpuTemp}"
  ];

  single = mkConky [
    "CPU: $(cpuload) - ${cexpr "cpu" [ "cpu0" ]}%"
    "MEM: \\$mem/\\$memmax - \\$memperc%"
    "NET: ${mkNetInfo "$primary_netdev"}"
    "TEMP - CPU: $(cputemp)"
  ];
}
