This directory contains various NixOS modules.

If you add a module here, make sure that you define all options using a
`vuizvui.*` namespace, so that the documentation is generated and you don't
clash with modules from upstream [nixpkgs](https://github.com/NixOS/nixpkgs).

For writing modules, make sure to categorize them accordingly:

<table>
  <tr>
    <th>profiles</th>
    <td>Options for a specific domain (like for example
        `desktop`, `router`, `music`, ...)
    </td>
  </tr>
  <tr>
    <th>programs</th>
    <td>Program-specific configuration options</td>
  </tr>
  <tr>
    <th>services</th>
    <td>Modules that implement systemd services</td>
  </tr>
  <tr>
    <th>system</th>
    <td>Everything system or hardware-related (like for example kernel)</td>
  </tr>
  <tr>
    <th>tasks</th>
    <td>Various one-shot services</td>
  </tr>
</table>

If a module is highly specific for your own configuration, use the same
categories but put them under `user/$category/$module`.

Don't forget to add your module to `module-list.nix`, but make sure you have
options in place to disable them by default.

## Module option reference

There also is a Hydra job for the currently available options specific to all of
the modules that are listed in `module-list.nix`:

https://headcounter.org/hydra/job/openlab/vuizvui/manual/latest/download/1/manual.html
