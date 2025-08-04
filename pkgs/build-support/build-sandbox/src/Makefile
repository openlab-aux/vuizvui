BINARIES = $(wildcard $(BINDIR)/*)
WRAPPERS = $(subst $(BINDIR),$(out)/bin,$(BINARIES))

OBJECTS = path-cache.o params.o setup.o
CFLAGS = -g -Wall -std=gnu11 -DFS_ROOT_DIR=\"$(out)\"
CXXFLAGS = -g -Wall -std=c++14 `pkg-config --cflags nix-store`
LDFLAGS = -Wl,--copy-dt-needed-entries `pkg-config --libs nix-store`

CFLAGS += -DEXTRA_NS_FLAGS="$(EXTRA_NS_FLAGS)"

ifdef FULL_NIX_STORE
CFLAGS += -DFULL_NIX_STORE
else
OBJECTS += nix-query.o
NIX_VERSION = `pkg-config --modversion nix-store | \
               sed -e 's/^\([0-9]\+\)\.\([0-9][0-9]\).*/\1\2/' \
                   -e 's/^\([0-9]\+\)\.\([0-9]\).*/\10\2/'`
CXXFLAGS += -DNIX_VERSION=$(NIX_VERSION)
endif

ifdef BINSH_EXECUTABLE
CFLAGS += -DBINSH_EXECUTABLE=\"$(BINSH_EXECUTABLE)\"
endif

all: $(OBJECTS)

$(out)/bin/%: CFLAGS += -DWRAPPED_PROGNAME=\"$(@F)\"
$(out)/bin/%: CFLAGS += -DWRAPPED_PATH=\"$(BINDIR)/$(@F)\"
$(out)/bin/%: $(OBJECTS)
	mkdir -p $(out)/bin
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $? sandbox.c

.PHONY: install
install: $(WRAPPERS)
