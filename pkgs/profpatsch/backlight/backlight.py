#!/usr/bin/env python3
# xbacklight uses a linear percentage,
# but the backlight intensity is actually logarithmic *facepalm*.
# So we read the current "percentage" given by xbacklight
# and calculate the next step, base 2.

import subprocess as sub
import math
import sys

xbacklight = "xbacklight"

def usage():
    print("usage: backlight [inc|dec]", file=sys.stderr)
    sys.exit(1)

# read current value
current_backlight = float(sub.run(args=[xbacklight, "-get"], stdout=sub.PIPE).stdout.strip())
# find the actual value, base 2
current_val = round(math.sqrt(current_backlight))

if len(sys.argv) == 1: usage()
else:
    mode = sys.argv[1]

# modify actual value
if mode == "inc":
    new_val = current_val + 1
elif mode == "dec":
    new_val = current_val - 1
else:
    usage()

# clamp value
new_backlight = min(10, max(0, new_val))

# pow it again and set
sub.run([xbacklight, "-set", str(math.pow(new_backlight, 2))])
