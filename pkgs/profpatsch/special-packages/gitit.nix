{ pkgs }:

let
  gitit = import (pkgs.fetchFromGitHub {
    owner = "Profpatsch";
    repo = "gitit";
    rev = "bcfba01472f09abec211c3509ec33726629068ea";
    sha256 = "sha256-v8s6GN4FTCfcsAxLdaP3HBYIDrJlZeKv+TOiRRt4bf4=";
  }) { inherit pkgs; };
in
  pkgs.haskell.lib.doJailbreak gitit
