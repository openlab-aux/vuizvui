{ xterm }:

xterm.overrideAttrs (drv: {
  patches = [ ./set-colorterm.patch ];
})
