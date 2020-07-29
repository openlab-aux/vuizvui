{ lib, runCommandLocal, writeText, python3, libnotify, bc, sfttime }:

let
  name = "display-infos-0.1.0";
  script = writeText (name + "-script") ''
    #!@python3@

    import sys
    import glob
    import subprocess as sub
    import os.path as path
    import statistics as st

    def readint(fn):
        with open(fn, 'r') as f:
            return int(f.read())

    def seconds_to_sft(secs):
        p = sub.Popen(["@bc@", "-l"], stdin=sub.PIPE, stdout=sub.PIPE)
        (sft, _) = p.communicate(input="scale=2; obase=16; {} / 86400\n".format(secs).encode())
        p.terminate()
        return str(sft.strip().decode())

    charging = readint("/sys/class/power_supply/AC/online")

    full = 0
    now  = 0
    # this is "to charged" if charging and "to empty" if not
    seconds_remaining = 0
    for bat in glob.iglob("/sys/class/power_supply/BAT*"):

        # these files might be different for different ACPI/battery providers
        # see the full list in acpi.c of the acpi(1) tool
        # unit: who knows
        full += readint(path.join(bat, "energy_full"))
        now  += readint(path.join(bat, "energy_now" ))
        # in unit?/hours, hopefully the same unit as above
        # ACPI is a garbage fire
        current_rate = readint(path.join(bat, "power_now"))

        if current_rate == 0:
          continue
        elif charging:
          seconds_remaining += 3600 * (full - now) / current_rate
        else:
          seconds_remaining += 3600 * now / current_rate

    bat = round( now/full, 2 )
    ac = "⚡ " if charging else ""
    sft_remaining = seconds_to_sft(seconds_remaining)
    date = sub.run(["date", "+%d.%m. %a %T"], stdout=sub.PIPE).stdout.strip().decode()
    dottime = sub.run(["date", "--utc", "+%H·%M"], stdout=sub.PIPE).stdout.strip().decode()
    sftdate = sub.run(["@sfttime@"], stdout=sub.PIPE).stdout.strip().decode()
    notify = "BAT: {percent}% {ac}{charge}| {date} | {sftdate} | {dottime}".format(
      percent = int(bat*100),
      ac = ac,
      charge = "{} ".format(sft_remaining) if seconds_remaining else "",
      date = date,
      sftdate = sftdate,
      dottime = dottime
    )
    print(notify)
  '';

in
  with lib; runCommandLocal "display-infos" {
    meta.description = "Script to display time & battery";
  } ''
    substitute ${script} script \
      --replace "@python3@" "${getBin python3}/bin/python3" \
      --replace "@bc@" "${getBin bc}/bin/bc" \
      --replace "@sfttime@" "${getBin sfttime}/bin/sfttime"
    install -D script $out/bin/display-infos
  ''
