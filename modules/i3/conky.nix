{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

let
  mkConfig = text: pkgs.writeText "conkyrc" ''
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
    ${text}
  '';

  cexpr = name: args: "\${${name} ${concatStringsSep " " args}}";

  primaryNetInterface = "enp0s25";

  mkCpuLoad = threads: let
    mkThread = thread: (cexpr "cpu" [ "cpu${toString thread}" ]) + "%";
    loads = map mkThread (range 1 threads);
  in concatStringsSep " " loads;

  mkCpuTemp = cores: let
    mkCore = core: (cexpr "platform" [
      "coretemp.0"
      "temp"
      "${toString (core + 1)}"
    ]) + "C";
    temps = map mkCore (range 1 cores);
  in concatStringsSep " " temps;

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
    cfg = mkConfig text;
  in pkgs.writeScript "conky-run.sh" ''
    #!${pkgs.stdenv.shell}
    ${conky}/bin/conky -c "${cfg}"
  '';

in {
  left = mkConky [
    "CPU: ${mkCpuLoad 8} - ${cexpr "cpu" [ "cpu0" ]}%"
    "MEM: $mem/$memmax - $memperc%"
    "SWAP: $swap/$swapmax $swapperc%"
  ];

  right = mkConky [
    "NET: ${mkNetInfo primaryNetInterface}"
    "DF: ${mkDiskFree "/"}"
    "LAVG: $loadavg"
    "TEMP - CPU: ${mkCpuTemp 4} - GPU: ${gpuTemp} - OUTSIDE: ${weather}"
  ];
}
