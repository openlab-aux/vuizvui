#!/usr/bin/env execlineb
elgetpositionals
ifelse { importas -S -D "" TERM eltest $TERM = dumb } {
  ifelse { if { eltest $# = 0 } eltest -t 0 } {
    foreground { fdmove -c 1 2 printf "%s: no files given and stdin is a terminal\n" $0 }
    exit 100
  }
  # rendered man pages traditionally use backspaces to embolden the section names
  # dumb terminals that don't implement them are left with character repeats
  pipeline { cat $@ } col -b
}
less -R $@
