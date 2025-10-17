# Profpatsch Packages - ReadTree Migration

## Current State

✅ **Phase 1 Complete** (October 2025)
- Successfully migrated to readTree for package discovery
- Eliminated ~200 lines of manual package imports and callPackage boilerplate
- All subdirectory packages (nman/, xdg-open/, etc.) now auto-discovered
- Added `...` to ~30 package function signatures for readTree compatibility
- Updated internal package cross-references to use `profpatsch.packageName` pattern

## Phase 2: Utility Function Extraction

### Goal

Extract utility functions from `default.nix` so that:
1. Utilities become auto-discoverable via readTree (no manual imports)
2. All packages reference them via `profpatsch.utils.*` namespace
3. `default.nix` becomes minimal (<100 lines)

### Key Insight: readTree Auto-Discovery

**How readTree handles directories:**
- If a directory has NO `default.nix`, readTree auto-discovers all `.nix` files as individual packages
- Files should use camelCase names for functions (e.g., `getBins.nix`, not `get-bins.nix`)
- Packages reference them as `profpatsch.utils.getBins`, `profpatsch.utils.netstring.toNetstring`, etc.
- Utilities are NOT passed via function arguments anymore

### Phase 2a: Extract Simple Utilities (IN PROGRESS)

**Files created:**
```
utils/
├── getBins.nix              # { lib, ... }: drv: xs: ...
├── binify.nix               # { pkgs, lib, ... }: { exe, name }: ...
├── writeHaskellInterpret.nix  # { pkgs, lib, ... }: nameOrPath: { withPackages ? ... }: content: ...
└── netstring.nix            # { lib, ... }: { toNetstring = ...; toNetstringList = ...; toNetstringKeyVal = ...; }
```

**What needs to be done:**

1. **Update all package files** to use `profpatsch.utils.*`:
   - Remove `getBins`, `binify`, `writeHaskellInterpret`, `toNetstring*` from function args
   - Add `profpatsch` to function args if not present
   - Replace all usages:
     - `getBins` → `profpatsch.utils.getBins`
     - `binify` → `profpatsch.utils.binify`
     - `writeHaskellInterpret` → `profpatsch.utils.writeHaskellInterpret`
     - `toNetstring` → `profpatsch.utils.netstring.toNetstring`
     - `toNetstringList` → `profpatsch.utils.netstring.toNetstringList`
     - `toNetstringKeyVal` → `profpatsch.utils.netstring.toNetstringKeyVal`

2. **Files to update** (~16 files found via grep):
   - pkgs/profpatsch/read-qr-code-from-camera.nix
   - pkgs/profpatsch/execline/e.nix
   - pkgs/profpatsch/nix-tools.nix
   - pkgs/profpatsch/deploy.nix
   - pkgs/profpatsch/read-qr-code.nix
   - pkgs/profpatsch/xdg-open/default.nix
   - pkgs/profpatsch/profpatsch.de/default.nix
   - pkgs/profpatsch/backup/default.nix
   - pkgs/profpatsch/gpg-private-offline-key/default.nix
   - pkgs/profpatsch/query-album-streams/default.nix
   - pkgs/profpatsch/youtube2audiopodcast/default.nix
   - pkgs/profpatsch/xrandr.nix
   - pkgs/profpatsch/text-letter.nix
   - pkgs/profpatsch/write-rust.nix
   - pkgs/profpatsch/query-album-streams/last-fm-api.nix
   - pkgs/profpatsch/execline/symlink.nix

3. **Update default.nix:**
   - Keep manual imports of utilities at the top (before readTree.fix) for use within default.nix itself
   - Remove utilities from `utilityArgs` (they'll be auto-discovered and referenced via profpatsch.utils.*)
   - Remove backward compatibility exports at the bottom

4. **Test:**
   ```bash
   nix-build -A pkgs.profpatsch.nman
   nix-build -A pkgs.profpatsch.xdg-open
   nix-build -A pkgs.profpatsch.backlight
   ```

5. **Commit incrementally**

### Phase 2b: Extract Run-Execline Functions

**Current location:** `runExeclineFns` in default.nix (uses getBins internally)

**Plan:**
- After Phase 2a completes, `runExeclineFns` can use `self.utils.getBins`
- Consider extracting to `execline/run-execline-fns.nix` or similar
- Handle circular dependency with testing package

**Complexity:** Medium (circular dependency with testing.drvSeqL)

### Phase 2c: Extract or Vendor nixperiments

**Current:** External GitHub dependency
- Provides: match, script, drvSeq, drvSeqL, withTests, filterSourceGitignoreWith, readGitignoreFile
- Used by: writeRust, testing, multiple packages

**Options:**
1. Keep as external import (current state)
2. Vendor into repo under `profpatsch/nixperiments/`

**Decision needed:** Discuss with maintainer

### Phase 2d: Final Cleanup

**Target default.nix structure:**
```nix
{ stdenv, lib, pkgs, sternenseemann, lazy-packages }:

let
  readTree = import ../../lib/readTree {};

  # Manual imports for use WITHIN default.nix only
  # (These are also auto-discovered by readTree for use by other packages)
  getBins = import ./utils/getBins.nix { inherit lib; };

  # ... other setup code that needs utilities ...

  utilityArgs = {
    inherit stdenv lib pkgs sternenseemann lazy-packages homeRepo;
    # NO utilities here - they're auto-discovered by readTree
    inherit (nixperiments) match script drvSeq drvSeqL withTests filterSourceGitignoreWith readGitignoreFile;
    inherit (runExeclineFns) runExecline runExeclineLocal runExeclineLocalNoSeqL;
    inherit (writeExeclineFns) writeExecline writeExeclineBin;
    inherit (writeRust) writeRustSimple writeRustSimpleBin writeRustSimpleLib;
    inherit (sandboxFns) sandbox runInEmptyEnv;
    inherit haskellPackagesProfpatsch testing;
    inherit (sternenseemann) temp;
  };

in readTree.fix (self: let
  discovered = readTree {
    path = ./.;
    args = utilityArgs // { profpatsch = self; };
  };

  specialPackages = rec {
    # Only non-discoverable packages
    inherit (homeRepo.users.Profpatsch) lyric alacritty;
    rust-deps = import ./rust-deps.nix { inherit (pkgs) buildRustCrate; };
    # ... etc
  };

in discovered // specialPackages
# NO backward compatibility exports - use profpatsch.utils.* directly
)
```

**Target:** <100 lines

### Benefits After Migration

- **Auto-discovery:** Utilities discovered automatically like packages
- **Consistent namespace:** All utilities under `profpatsch.utils.*`
- **No argument pollution:** Packages don't need to list all utility dependencies
- **Clear structure:** `default.nix` only contains bootstrap and special cases
- **Type safety:** IDE/LSP can discover `profpatsch.utils.*` attributes

### Current Statistics (Phase 2a)

- **Lines in default.nix:** ~170 lines (after Phase 1)
- **Utility function definitions:** ~50 lines extracted so far
- **Files using utilities:** ~16 files need updating
- **Goal:** Reduce default.nix to <100 lines total

### Testing Strategy

For each phase:
1. Extract utilities to separate files
2. Update all package references
3. Update default.nix
4. Test critical packages
5. Commit with descriptive message

### Notes

- `default.nix` itself will still manually import utilities it needs (for bootstrap)
- Other packages will reference them via `profpatsch.utils.*` (auto-discovered)
- netstring.nix exports a set, so references become `profpatsch.utils.netstring.toNetstring`
- No backward compatibility exports - force migration to new namespace
