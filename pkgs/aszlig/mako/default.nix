{ mako }:

mako.overrideAttrs (drv: {
  patches = (drv.patches or []) ++ [ ./config.patch ];
})
