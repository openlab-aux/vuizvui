/*

  This module is the result of a more serious endeavour into
  using mandoc as a complete replacement for man-db. Using
  mandoc for viewing manual pages already kind of works by
  just adding mandoc to environment.systemPackages — which
  results in a lot of ugly environment clashes.

  man-db and mandoc only partially implement the same interface,
  so it's not just a matter of switching out the used package
  in documentation.man. However, a separate module is entirely
  possible and even simpler than the one for man-db.

  However with generating the mandoc.db for apropos(1),
  whatis(1) and -k of man(1), we hit a bump in the road:
  makewhatis(8) does some sanity checking of the indexed
  man pages which also includes checking that the realpath(3)
  of any given man page is below the indexed directory.
  This means that all man pages which are located in /nix/store
  (i. e. all of them) will get skipped.

  This behavior is easily patched and there is also a similar
  feature intended for homebrew which could be (ab)used. While
  I try to get this particular fix/feature into upstream mandoc,
  the patched package and the experimental module reside in
  vuizvui. When everything is sorted, I'll (hopefully) be able
  to contribute this module to nixpkgs upstream as well.

  Therefore this is also a user module in vuizvui despite its
  non-me-specific nature — I hope this will only be temporary!
  I do promise to fix your system configurations should you
  use this particular module, though.

*/
{ config, lib, pkgs, ... }:

let

  inherit (pkgs.vuizvui.sternenseemann)
    mandoc
    ;

  inherit (pkgs.vuizvui.profpatsch)
    getBins
    ;

  bins = getBins mandoc [ "makewhatis" ];

  cfg = config.vuizvui.user.sternenseemann.documentation.mandoc;
  docCfg = config.documentation;

in {
  options = {
    vuizvui.user.sternenseemann.documentation.mandoc = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to install the man utilities from
          <literal>mandoc</literal> as well as manual
          pages for installed packages from their
          <literal>man</literal> outputs.
          This can be used as a drop-in replacement
          for <literal>documentation.man</literal>
          with a few smaller differences.
        '';
      };

      manPath = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ "share/man" ];
        example = lib.literalExample "[ \"share/man\" \"share/man/fr\" ]";
        description = ''
          Change the manpath, i. e. the directories where man looks
          for section-specific directories of man pages.
          You only need to change this setting if you want non-english
          man pages or a combination of different languages. All values
          must be strings that are a valid path from the target prefix
          (without including it). The first value given takes priority.
        '';
      };

      generateCaches = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Whether to generate a <literal>mandoc.db</literal> indexing
          the installed man pages for <literal>apropos(1)</literal>,
          <literal>whatis(1)</literal> and the <literal>-k</literal>
          option of <literal>man(1)</literal> using
          <literal>makewhatis(8)</literal>.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {

    assertions = [
      {
        assertion = !(cfg.enable && docCfg.man.enable);
        message = ''
          vuizvui.user.sternenseemann.documentation.mandoc is a
          replacement for documentation.man meaning they can't be
          enabled at the same time.
        '';
      }
    ];

    environment = {
      # globally install man pages
      pathsToLink = [ "/share/man" ];
      extraOutputsToInstall = [ "man" ]
        ++ lib.optional docCfg.dev.enable "devman";
      # tell mandoc about man pages
      systemPackages = [ mandoc ];
      etc."man.conf".text = lib.concatMapStrings
        (path: "manpath /run/current-system/sw/${path}\n")
        cfg.manPath;
      # create mandoc.db for whatis(1), apropos(1) and man(1) -k
      # TODO(sterni): remove -p for production™
      extraSetup = lib.optionalString cfg.generateCaches ''
        ${bins.makewhatis} -p -T utf8 \
          ${lib.concatMapStringsSep " "
              (path: "\"$out/${path}\"")
              cfg.manPath}
      '';
    };
  };
}
