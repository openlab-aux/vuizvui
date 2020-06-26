let

  netstring = tag: suffix: s:
    "${tag}${toString (builtins.stringLength s)}:${s}${suffix}";

  unit = "u,";

  n1 = b: if b then "n1:1," else "n1:0,";

  n = i: n: netstring "n" ",";
  i = i: n: netstring "i" ",";

  n3 = n 3;
  n6 = n 6;
  n7 = n 7;

  i3 = i 3;
  i6 = i 6;
  i7 = i 7;

  text = netstring "t" ",";
  binary = netstring "b" ",";

  tag = key: val: netstring "<" "|" key + val;

  concatStrings = builtins.concatStringsSep "";

  record = lokv: netstring "{" "}"
    (concatStrings (map (kv: tag kv.key kv.val) lokv));

  list = l: netstring "[" "]" (concatStrings l);

in {
  inherit
    unit
    n1
    n3
    n6
    n7
    i3
    i6
    i7
    text
    binary
    tag
    record
    list
    ;
}
