{ writeScriptBin, python3, nix }:

writeScriptBin "nlast" ''
  #!${python3.interpreter}
  from os import execl
  from functools import partial
  from pathlib import Path

  newest = partial(max, key=lambda entry: entry.stat().st_mtime)
  prefix = newest(Path('/nix/var/log/nix/drvs').iterdir())
  drvname = prefix.name + newest(prefix.iterdir()).stem
  execl('${nix}/bin/nix', 'nix', 'log', Path('${builtins.storeDir}') / drvname)
''
