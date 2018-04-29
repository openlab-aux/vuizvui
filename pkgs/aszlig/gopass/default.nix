{ gopass }:

gopass.overrideAttrs (drv: {
  patches = [
    ./ascii-symbols.patch
    ./use-color-in-pager.patch
  ];
})
