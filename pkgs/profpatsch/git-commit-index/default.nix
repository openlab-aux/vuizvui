{ cdb, git, writeShellScriptBin, symlinkJoin, lib, script, runCommandLocal }:

let
  pathPlus = ''PATH="$PATH:${lib.makeBinPath [cdb git]}"'';

  mkIndex = script.withOptions {
    name = "git-commit-index-create";
    synopsis = "Create a git commit index for all .git directories, recursively.";

    options = {
      index = {
        description = "Location of the index. Must not exist.";
        checks = [ script.optionChecks.emptyPath ];
      };
    };

    extraArgs = {
      description = "List of root directories to recursively search";
      name = "DIRS";
      checks = [ script.argsChecks.allAreDirs ];
    };

    script = ''
      ${pathPlus}
      source ${./lib.sh}
      mkdir "$index"
      for root in "$@"; do
        for repo in $(findRepos "$root"); do
          genIndex "$index" "$repo"
        done
      done
    '';
  };

  queryIndex = script.withOptions {
    name = "git-commit-index-query";

    synopsis = "Search a git commit index for a rev, return the repository that rev is in.";

    options = {
      index = {
        description = "Location of the populated index.";
        checks = [ script.optionChecks.isDir ];
      };
      rev = {
        description = "Full git rev hash to look up";
        # TODO: check that it is a commit hash?
        checks = [];
      };
    };

    script = ''
      ${pathPlus}
      source ${./lib.sh}

      query "$index" "$rev"
    '';
  };

  # magitOpenCommit = writeShellScriptBin "git-commit-index-magit-open" ''
  #   ${pathPlus}
# emacsclient -e '(let ((default-directory "/home/philip/kot/work/tweag/lorri"))
#                                    (magit-mode-setup #\'magit-revision-mode "43a72bc220dae8f34bd0d889f2b1b1ce2a6092b7" nil nil nil))'

in
  runCommandLocal "git-commit-index" {} ''
    install -D ${mkIndex} $out/bin/git-commit-index-create
    install -D ${queryIndex} $out/bin/git-commit-index-query
  ''
