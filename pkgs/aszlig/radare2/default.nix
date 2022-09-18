{ radare2 }:

radare2.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./att-syntax-by-default.patch ];
})
