{ tvlSrc ? builtins.fetchGit {
    name = "tvl-depot";
    url = "https://code.tvl.fyi";
    rev = "8e85079012dd9869753d0f60849f83b8f3cb6459"; # 2022-03-27
    ref = "canon";
  }
, pkgs
}:

import tvlSrc {
  nixpkgsBisectPath = pkgs.path; # TODO: does this improve eval time significantly?
}
