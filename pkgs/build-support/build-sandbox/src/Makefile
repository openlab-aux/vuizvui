BINARIES = $(wildcard $(BINDIR)/*)
WRAPPERS = $(subst $(BINDIR),$(out)/bin,$(BINARIES))

NIX_VERSION = `pkg-config --modversion nix-main | \
               sed -e 's/^\([0-9]\+\)\.\([0-9]\+\).*/\1\2/'`

OBJECTS = nix-query.o path-cache.o params.o setup.o

CFLAGS = -g -Wall -std=gnu11 -DFS_ROOT_DIR=\"$(out)\"
CXXFLAGS = -g -Wall -std=c++14 `pkg-config --cflags nix-main`
CXXFLAGS += -DNIX_VERSION=$(NIX_VERSION)
LDFLAGS = `pkg-config --libs nix-main`

all: $(OBJECTS)

$(out)/bin/%: CFLAGS += -DWRAPPED_PROGNAME=\"$(@F)\"
$(out)/bin/%: CFLAGS += -DWRAPPED_PATH=\"$(BINDIR)/$(@F)\"
$(out)/bin/%: $(OBJECTS)
	mkdir -p $(out)/bin
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $? sandbox.c

.PHONY: install
install: $(WRAPPERS)
