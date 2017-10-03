BINARIES = $(wildcard $(BINDIR)/*)
WRAPPERS = $(subst $(BINDIR),$(out)/bin,$(BINARIES))

OBJECTS = get-closure.o params.o setup.o

CFLAGS = -Wall -std=gnu11 -DFS_ROOT_DIR=\"$(out)\"
CXXFLAGS = -Wall -std=c++14 `pkg-config --cflags nix-main`
LDFLAGS = `pkg-config --libs nix-main`

all: $(OBJECTS)

$(out)/bin/%: CFLAGS += -DWRAPPED_PROGNAME=\"$(@F)\"
$(out)/bin/%: CFLAGS += -DWRAPPED_PATH=\"$(BINDIR)/$(@F)\"
$(out)/bin/%: $(OBJECTS)
	mkdir -p $(out)/bin
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $? sandbox.c

.PHONY: install
install: $(WRAPPERS)
