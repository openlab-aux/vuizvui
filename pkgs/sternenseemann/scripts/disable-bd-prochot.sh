#!/bin/sh
set -eu

# This not documented publicly to my knowledge. The register address
# seems to be CPU model specific, technically, so double check yourself.
# https://reverseengineering.stackexchange.com/questions/22239/undocumented-model-specific-register-on-broadwell-microarchitecture
# https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
readonly MSR_POWER_CTL=0x1FC
readonly ENABLE_BD_PROCHOT=$((1 << 0))

# Verify we have the permissions to read
rdmsr -a "$MSR_POWER_CTL" >/dev/null || exit $?

if [ "$(rdmsr --unsigned -a "$MSR_POWER_CTL" | sed 's/^.\+: //' | uniq | wc -l)" = "1" ]; then
  readonly current_value="$(rdmsr --unsigned "$MSR_POWER_CTL")"
else
  printf \
    "%s: not all CPUs have the same value for MSR %s. Refusing to continue\n" \
    "$0" \
    "$MSR_POWER_CTL" >&2
  exit 1
fi

# wrmsr uses strtoul(3) with base=0, so it'll parse our decimal
readonly new_value="$(("$current_value" & ~("$ENABLE_BD_PROCHOT")))"

printf "Read MSR %s: %s\n" "$MSR_POWER_CTL" "$current_value"
printf "Set  MSR %s: %s\n" "$MSR_POWER_CTL" "$new_value"
wrmsr -a "$MSR_POWER_CTL" "$new_value"
