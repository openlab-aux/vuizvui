#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p curl python3

import sys
import subprocess as sub
import datetime

IP = "46.252.18.154"
DOMAIN = "profpatsch.de"
PROTOCOL = "http"
FILE = "/stuff/speedtest.rng"

HOST_BIN = "host"
PING_BIN = "ping"

dns = 0 == sub.run([HOST_BIN, "-W1", DOMAIN], stdout=sub.DEVNULL).returncode

hostname = DOMAIN if dns else IP

ping = 0 == sub.run([PING_BIN, "-w1", "-W1", "-c1", hostname],
                    stdout=sub.DEVNULL).returncode

bytes_per_sec = 0
if ping == True:
    res = sub.run(["curl", "--silent", PROTOCOL + "://" + hostname + FILE,
                   "--write-out", "\n%{speed_download}"],
                  stdout=sub.PIPE, stderr=sub.PIPE)
    if res.returncode != 0:
        sys.exit("download failed unexpectedly. curl outputs:\n" + res.stderr)
    else:
        # the last line is the download speed
        out = res.stdout.split(b"\n")[-1].strip()
        try:
            bytes_per_sec = float(out)
        except ValueError:
            sys.exit("last line of curl was no float (bytes per sec), but:\n" +
                     out[0:100] + "\nthere were " + len(out) + " lines in the output")

# some yaml-like output
def bool_(b):
    return "true" if b else "false"

print("---")
print("date: " + str(datetime.datetime.now()))
print("dns: " + bool_(dns))
print("ping: " + bool_(ping))
print("download_speed: {}".format(int(bytes_per_sec)))
