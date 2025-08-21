#!/usr/bin/env -S gawk -f
# Needs GNU awk for match() saving into an array

match($0, /^# group: (.+)/, caps)    { group = caps[1]; subgroup = "" }
match($0, /^# subgroup: (.+)/, caps) { subgroup = " → " caps[1] }
match($0, /^(([0-9a-fA-F]+ )+)\s*; [^#]+# ([^ ]+) E[0-9.]+ (.+)/, caps) {
  printf "%s %s (%s%s)\n", caps[3], caps[4], group, subgroup
}
