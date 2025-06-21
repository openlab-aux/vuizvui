#!/usr/bin/env BQN
# https://www.swatch.com/en-en/internet-time.html
# https://en.wikipedia.org/wiki/Swatch_Internet_Time

# Convert Unix Timestamp to Internet Time
#    It •UnixTime @
# 963
It ⇐ {⌊86.4÷˜86400|3600+𝕩}
# Format Internet Time for Output
#    FmtBeats 43
# "@043"
FmtBeats ⇐ {'@'∾'0'+10|⌊𝕩÷10⋆⌽↕3}

# TODO(sterni): make importable as a library
•Out FmtBeats It •UnixTime @
