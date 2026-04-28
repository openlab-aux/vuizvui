{ lib, ... }:

# Takes a derivation and a list of binary names
# and returns an attribute set of `name -> path`.
# The list can also contain renames in the form of
# { use, as }, which goes `as -> usePath`.
drv: xs:
  let f = x:
    # TODO: typecheck
    let x' = if builtins.isString x then { use = x; as = x; } else x;
    in {
      name = x'.as;
      value = "${lib.getBin drv}/bin/${x'.use}";
    };
  in builtins.listToAttrs (builtins.map f xs)
