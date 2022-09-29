{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "b593f6922c3b31f2456cb8885d80af3789a380b3"; # 2022-09-29
    ref = "canon";
  }
, pkgs
}:

import tvlSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
}
