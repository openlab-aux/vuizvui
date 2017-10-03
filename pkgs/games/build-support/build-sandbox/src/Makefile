BINARIES = $(wildcard $(BINDIR)/*)
WRAPPERS = $(subst $(BINDIR),$(out)/bin,$(BINARIES))

CFLAGS = -Wall -std=gnu11 -DFS_ROOT_DIR=\"$(out)\"
CXXFLAGS = -Wall -std=c++14 `pkg-config --cflags nix-main`
LDFLAGS = `pkg-config --libs nix-main`

all: get-closure.o

$(out)/bin/%: CFLAGS += -DWRAPPED_PROGNAME=\"$(@F)\"
$(out)/bin/%: CFLAGS += -DWRAPPED_PATH=\"$(BINDIR)/$(@F)\"
$(out)/bin/%: get-closure.o
	mkdir -p $(out)/bin
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $? sandbox.c

.PHONY: install
install: $(WRAPPERS)
