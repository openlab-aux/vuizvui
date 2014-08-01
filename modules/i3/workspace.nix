{ name, lib, config, ... }:

with lib;

let
  finalLabel =
    if config.label == null then name
    else config.labelPrefix + config.label;

  mkDoc = anchor: "http://i3wm.org/docs/userguide.html#${anchor}";
in
{
  options = {
    labelPrefix = mkOption {
      type = types.str;
      default = "";
      example = "666: ";
      description = ''
        The value that will be put in front of the <option>label</option>.
        So if you have a label called <replaceable>bar</replaceable> and a
        <option>labelPrefix</option> called <replaceable>foo</replaceable> the
        label for the workspace will be <replaceable>foobar</replaceable>.
      '';
    };

    label = mkOption {
      type = types.nullOr types.str;
      default = name;
      description = ''
        The label of this workspace, which is its name by default. If the value
        is <replaceable>null</replaceable>, the resulting label of the workspace
        is just its name and no <option>labelPrefix</option> is applied.
      '';
    };

    assign = mkOption {
      type = types.listOf types.attrs;
      default = [];
      example = [
        { class = "^Chromium(?:-browser)?\$"; }
        { instance = "^gajim\$"; }
      ];
      description = let
        anchor = "_automatically_putting_clients_on_specific_workspaces";
      in ''
        Assign windows to this specific workspace using the attribute names
        described by <link xlink:href="${mkDoc anchor}"/>.
      '';
    };

    head = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The XRandR head this workspace will be assigned to.
      '';
    };

    keys = let
      commonDesc = ''
        The <replaceable>$mod</replaceable> placeholder represents the default
        modifier key. Details about the syntax of key combinations can be found
        at <link xlink:href="${mkDoc "keybindings"}"/>.
      '';
    in {
      switchTo = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "$mod+1";
        description = ''
          Key combination to switch to this workspace.
        '' + commonDesc;
      };

      moveTo = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "$mod+Shift+exclam";
        description = ''
          Key combination to move a container to this workspace.
        '' + commonDesc;
      };
    };

    config = mkOption {
      type = types.lines;
      default = "";
      description = ''
        Raw configuration options for this workspace.
      '';
    };
  };

  config.config = let
    mkAssign = mapAttrsToList (criteria: value: "${criteria}=\"${value}\"");
    mkSym = sym: rest: optionalString (sym != null) "bindsym ${sym} ${rest}";
  in ''
    ${optionalString (config.head != null) ''
    workspace "${finalLabel}" output ${config.head}
    ''}
    ${mkSym config.keys.switchTo "workspace \"${finalLabel}\""}
    ${mkSym config.keys.moveTo "move workspace \"${finalLabel}\""}
    ${concatMapStrings (assign: ''
    assign [${concatStringsSep " " (mkAssign assign)}] ${finalLabel}
    '') config.assign}
  '';
}
