#!/usr/bin/env bash

set -uo pipefail

hidePaths=("/home/lukas/src")
roPaths=("/home/lukas/src/tcl/ma")
umount=false

while getopts "uh" opt; do
  case "$opt" in
  u)
    umount=true
    ;;
  *)
    echo "focus [-u]" >&2
    exit 100
    ;;
  esac
done

x() {
  echo '$' "$@" >&2
  "$@"
}

if $umount; then
  for ro in "${roPaths[@]}"; do
    x umount "$ro"
  done

  for hide in "${hidePaths[@]}"; do
    x umount "$hide"
  done
else
  set -e

  for hide in "${hidePaths[@]}"; do
    x mount -t tmpfs none "$hide"
  done
  
  TMPMOUNT="$(mktemp -d)"
  cleanup() {
    x umount -q "$TMPMOUNT"
    x rmdir "$TMPMOUNT"
  }
  trap cleanup EXIT
  
  x mount --bind / "$TMPMOUNT"
  for ro in "${roPaths[@]}"; do
    x mkdir -p "$ro"
    x mount --bind "$TMPMOUNT/$ro" "$ro"
    x mount -o remount,bind,ro "$ro"
  done
fi