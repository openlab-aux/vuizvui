{ gopass }:

gopass.overrideAttrs (drv: {
  patches = [
    ./use-color-in-pager.patch
  ];
})
