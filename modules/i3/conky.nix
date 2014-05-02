{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

let
  baseConfig = pkgs.writeText "conkyrc" ''
    cpu_avg_samples 2
    net_avg_samples 2
    no_buffers yes
    out_to_console yes
    out_to_ncurses no
    out_to_stderr no
    extra_newline no
    update_interval 1.0
    uppercase no
    use_spacer none
    pad_percents 3
    use_spacer left
    TEXT
  '';

  cexpr = name: args: "\\\${${name} ${concatStringsSep " " args}}";

  primaryNetInterface = "enp0s25";

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
    "http://weather.noaa.gov/pub/data/observations/metar/stations/"
    "EDMA"
    "temperature"
  ]) + "C";

  mkConky = args: let
    time = cexpr "time" [ "%a %b %d %T %Z %Y" ];
    text = concatStringsSep " | " (args ++ singleton time);
    conky = pkgs.conky.override {
      weatherMetar = true;
    };
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
    ${conky}/bin/conky -c "${baseConfig}" -t "${text}"
  '';

in {
  left = mkConky [
    "CPU: $(cpuload) - ${cexpr "cpu" [ "cpu0" ]}%"
    "MEM: \\$mem/\\$memmax - \\$memperc%"
    "SWAP: \\$swap/\\$swapmax \\$swapperc%"
  ];

  right = mkConky [
    "NET: ${mkNetInfo primaryNetInterface}"
    "DF: ${mkDiskFree "/"}"
    "LAVG: \\$loadavg"
    "TEMP - CPU: $(cputemp) - GPU: ${gpuTemp} - OUTSIDE: ${weather}"
  ];
}
