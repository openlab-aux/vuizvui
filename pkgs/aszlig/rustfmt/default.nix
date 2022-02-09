{ rustfmt, runCommand, remarshal }:

let
  defaultConfig = {
    format_code_in_doc_comments = true;
    format_macro_matchers = true;
    format_strings = true;
    group_imports = "StdExternalCrate";
    match_block_trailing_comma = true;
    max_width = 79;
    newline_style = "Unix";
    normalize_doc_attributes = true;
    overflow_delimited_expr = true;
    reorder_impl_items = true;
    unstable_features = true;
    use_field_init_shorthand = true;
    use_try_shorthand = true;
    wrap_comments = true;
  };

in rustfmt.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./config.patch ];
  patchFlags = [ "-p1" "-d" "src/tools/rustfmt" ];
  DEFAULT_CONFIG_FILE = runCommand "rustfmt.conf" {
    nativeBuildInputs = [ remarshal ];
    value = builtins.toJSON defaultConfig;
    passAsFile = [ "value" ];
  } ''
    json2toml "$valuePath" "$out"
  '';
})
