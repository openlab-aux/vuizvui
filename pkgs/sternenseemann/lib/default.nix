{ lib }:

{
  # TODO(sterni): recurse into fset instead of set,
  #               should improve performance, but
  #               is a bit more annoying to implement
  #
  # Alter specific values of a nested attribute set
  # using functions specified in another attrset.
  #
  # mapAttrsByAttrs recurses into the first given attribute
  # set and checks if an attribute at the same location
  # exists in the other one. If yes, the function at that
  # location in the second attribute set is called with
  # the current path in the attribute set and the value
  # at that location in the first attribute set. If
  # no equivalent function is found in the second set,
  # the value is unchanged.
  #
  # The set returned is the first set with all values
  # altered in the described manner.
  #
  # Type:
  #
  #  mapAttrsByAttrs :: AttrSet -> AttrSet_f -> AttrSet
  #
  #    where AttrSet_f = either ([String] -> a -> a) AttrSet_f
  #
  # Example:
  #
  #  x = {
  #    ints = {
  #      hello = 13;
  #      world = 12;
  #    };
  #    strs = {
  #      hello = "hello";
  #      world = "world!";
  #    };
  #  };
  #  mapAttrsByAttrs x {
  #    ints = {
  #      hello = path: value: builtins.toString value;
  #    };
  #    strs = {
  #      hello = path: value: "${value}!";
  #    };
  #  }
  #  => {
  #    ints = {
  #      hello = "13";
  #      world = 12;
  #    };
  #    strs = {
  #      hello = "hello!";
  #      world = "world!";
  #    };
  #  }
  mapAttrsByAttrs = set: fset:
    let
      havef = path: builtins.length path > 0
        && lib.hasAttrByPath path fset;
      apply = path: val:
        let
          id = p: v: v;
        in
          (lib.attrByPath path id fset) path val;
      recurse = path: rset:
        let
          g = name: value:
            if builtins.isAttrs value && !(havef path)
            then recurse (path ++ [ name ]) value
            else apply (path ++ [ name ]) value;
      in
        lib.mapAttrs g rset;
    in
      recurse [] set;
}
