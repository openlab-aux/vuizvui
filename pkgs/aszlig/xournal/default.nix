{ xournal }:

xournal.overrideAttrs (attrs: {
  patches = (attrs.patches or []) ++ [ ./aspect-ratio.patch ];
})
