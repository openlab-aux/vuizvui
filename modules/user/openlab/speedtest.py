#!/usr/bin/env nix-shell
#!nix-shell -i python3 -p curl python3

import sys
import subprocess as sub
import datetime

IP = "163.172.44.192"
DOMAIN = "haku.profpatsch.de"
PROTOCOL = "https"
FILE = "/pub/well-known/speedtest-5M.rng"
SIZE = 5242880

HOST_BIN = "host"
PING_BIN = "ping"

v4 = 0 == sub.run([HOST_BIN, "-4", "-W1", DOMAIN], stdout=sub.DEVNULL).returncode
v6 = 0 == sub.run([HOST_BIN, "-6", "-W1", DOMAIN], stdout=sub.DEVNULL).returncode
dns = v4 or v6

ping = 0 == sub.run([PING_BIN, "-w1", "-W1", "-c1", DOMAIN if dns else IP],
                    stdout=sub.DEVNULL).returncode

bytes_per_sec = 0
if dns:
    res = sub.run(["curl", PROTOCOL + "://" + DOMAIN + FILE,
                   "--write-out", "\n%{size_download} %{speed_download}"],
                  stdout=sub.PIPE, stderr=sub.PIPE)
    if res.returncode != 0:
        sys.exit("download failed unexpectedly. curl outputs:\n{}".format(res.stderr))
    else:
        # the last line is the download speed
        out = res.stdout.split(b"\n")[-1].strip().split(b" ")
        try:
            download_size = int(out[0])
            if download_size != SIZE:
                sys.exit("download size should have been {} but is {}"
                         .format(SIZE, download_size))
            bytes_per_sec = float(out[1])
        except ValueError:
            sys.exit("last line of curl was no float (bytes per sec), but:\n" +
                     out[0:100] + "\nthere were " + len(out) + " lines in the output")

# some yaml-like output
def bool_(b):
    return "true" if b else "false"

print("---")
print("version: 0.2")
print("date: " + str(datetime.datetime.now()))
print("ping-v4: " + bool_(v4))
print("ping-v6: " + bool_(v6))
print("dns: " + bool_(dns))
print("download_speed: {}".format(int(bytes_per_sec)))
