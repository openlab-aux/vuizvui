{ pkgs, writeExecline }:

let

  # remove everything but a few selected environment variables
  runInEmptyEnv = keepVars:
    let
        importas = pkgs.lib.concatMap (var: [ "importas" "-i" var var ]) keepVars;
        # we have to explicitely call export here, because PATH is probably empty
        export = pkgs.lib.concatMap (var: [ "${pkgs.execline}/bin/export" var ''''${${var}}'' ]) keepVars;
    in writeExecline "empty-env" {}
         (importas ++ [ "emptyenv" ] ++ export ++ [ "${pkgs.execline}/bin/exec" "$@" ]);


  # lightweight sandbox; execute any command in an unshared
  # namespace that only has access to /nix and the specified
  # directories from `extraMounts`.
  sandbox = { extraMounts ? [] }:
    let
      pathsToMount = [
        "/nix"
        "/dev" "/proc" "/sys"
      ] ++ extraMounts;
      # chain execlines and exit immediately if one fails
      all = builtins.concatMap (c: [ "if" c ]);
      mount = "${pkgs.utillinux}/bin/mount";
      unshare = "${pkgs.utillinux}/bin/unshare";
      # this is the directory the sandbox runs under (in a separate mount namespace)
      newroot = pkgs.runCommandLocal "sandbox-root" {} ''mkdir "$out"'';
      # this runs in a separate namespace, sets up a chroot root
      # and then chroots into the new root.
      sandbox = writeExecline "sandbox" {} (builtins.concatLists [
        # first, unshare the mount namespace and make us root
        # -> requires user namespaces!
        [ unshare "--mount" "--map-root-user" ]
        (all
          # mount a temporary file system which we can chroot to;
          # we can use the fixed path newroot here, because the resulting
          # tmpfs cannot be seen from the outside world (we are in an unshared
          # mount )
          ([ [ mount "-t" "tmpfs" "container_root" newroot ] ]
          # now mount the file system parts we need into the chroot
          ++ builtins.concatMap
               (rootPath: [
                 [ "${pkgs.coreutils}/bin/mkdir" "-p" "${newroot}${rootPath}" ]
                 [ mount "--rbind" rootPath "${newroot}${rootPath}" ]
               ])
               pathsToMount))
        [ # finally, chroot into our new root directory
          "${pkgs.coreutils}/bin/chroot" newroot
          # drop root permissions, become user nobody;
          # This is because many programs don’t like to be root
          # TODO: this unshare does not work, because we don’t have
          # the right permissions to do that here, unfortunately :(
          # unshare "--user" "--"
          "$@"
        ]
      ]);
    in sandbox;

in {
  inherit sandbox runInEmptyEnv;
}
