{ callPackage, vim_configurable }:

{
  vim = callPackage ./vim { };
  nvim = callPackage ./nvim { };
}
