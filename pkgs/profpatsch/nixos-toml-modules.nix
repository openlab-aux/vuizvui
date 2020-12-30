{ lib }:

let
  tv = lib.traceValSeqN 3;
  parseSemver = s:
    let els = lib.splitString "." s;
    in assert (lib.assertMsg (builtins.length els == 3) "semver must be of the form maj.min.patch, was ${s}"); {
        major = lib.head els;
        minor = lib.head (lib.tail els);
        patch = lib.head (lib.tail (lib.tail els));
      };
  semver = major: minor: patch: { inherit major minor patch; };

  nixos-options-0_0_1 = {toml, tomlFileDir}:
    let
      toOption = { description, type, default }: lib.mkOption {
        inherit description default;
        # TODO: this only works for types which are not functions obviously
        type = lib.types.${type};
      };

      transformOptions =
        let isOptionAttrset = a: a ? description && a ? type && a ? default;
        in lib.mapAttrsRecursiveCond
          # TODO: if an option set had these fields for any reason,
          # we wouldn’t recognize it as an option.
          # Maybe there should be an escape hatch in the toml description?
          # We wouldn’t want the toml description to need an extra annotation for an option definition
          (attrs: !isOptionAttrset attrs)
          (_path: attrs:
            if isOptionAttrset attrs
            then toOption attrs
            else lib.id);

    in {
      options = transformOptions toml;
    };

  nixos-config-0_0_1 = {toml, tomlFileDir } : config:
    let
      configVariables = toml.configVariables or {};
      getSingleAttr = attrs: errWrongLength:
        let keys = lib.attrNames attrs;
            len = lib.length keys;
        in
        assert lib.assertMsg (len == 1)
          (errWrongLength { num = len; keys = keys; });
        let key = lib.head keys;
        in { name = key; value = attrs.${key}; };
      mapIfAttrs = attrKeys: f: attrs:
        # lib.attrNames is always sorted like (lib.sort lib.lessThan)
        if (lib.sort lib.lessThan attrKeys) == (lib.attrNames attrs)
        then { mapped = true; val = f attrs; }
        else { mapped = false; val = attrs; };

      transformConfigVariable = { _configVariable }:
        let var = getSingleAttr _configVariable
          ({ num, keys }: "_configVariable must have exactly one attribute, the config variable to match, but you gave ${toString num}: ${lib.generators.toPretty {} keys}");
        in
        assert lib.assertMsg (configVariables ? ${var.name})
          "_configVariable referenced ${var.name}, but you haven’t specified ${var.name} in [configVariables], only: ${lib.generators.toPretty {} toml.configVariables}";
        let
          initialPath = configVariables.${var.name};
          lastPathElem = var.value;
        in lib.getAttrFromPath (initialPath ++ [lastPathElem]) config;

      relativeStringToPath = parentPath: relString: parentPath + ("/" + relString);
      transformImports = map ({module}: relativeStringToPath tomlFileDir module);

      # recurse a structure of attrsets and lists recursively.
      # f takes the current value and returns a { recurse : bool, val : a}
      # where if recurse is true the result of f (val) will be recursed into further
      # and val is the returned value.
      mapValRecursive = f: val:
        let mapped = f val;
        in if !mapped.recurse then mapped.val
        else if lib.isAttrs mapped.val
          then lib.mapAttrs (_: mapValRecursive f) mapped.val
        else if lib.isList mapped.val
          then map (mapValRecursive f) mapped.val
        else mapped.val;

      transformConfig =
        { imports = transformImports toml.imports;
          config =
            mapValRecursive
              (val:
                 if lib.isAttrs val
                 then
                   let m = mapIfAttrs [ "_configVariable" ] transformConfigVariable val;
                   in { recurse = !m.mapped; val = m.val; }
                 else { recurse = true; inherit val; })
              (removeAttrs toml [ "imports" "configVariables" ]);
        };

    in transformConfig;

  readAnyToml = filePath:
    let
      toml = builtins.fromTOML (builtins.readFile filePath);
      parsers = {
        "nixos-options" = {
          "0.0.1" = nixos-options-0_0_1;
        };
        "nixos-config" = {
          "0.0.1" = nixos-config-0_0_1;
        };
      };
       # TODO: these errors are gonna be horrible …
    in parsers.${toml.module.type}.${toml.module.version} {
        toml = (removeAttrs toml [ "module" ]);
        tomlFileDir = builtins.dirOf filePath;
    };

in {
  inherit
    readAnyToml
    ;
}
