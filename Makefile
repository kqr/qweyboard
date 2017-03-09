MKDIR=mkdir -p
PLATFORM=x11
# Comment out to use the dummy platform instead
# PLATFORM=dummy_platform

BIN=qweyboard
BINDIR=bin
SRCDIR=src
BUILDDIR=build

CC=gnatmake
LANGVARIANT=-gnatW8
CFLAGS=-D $(BUILDDIR)

ifeq ($(PLATFORM),x11)
LIBS=-lX11 -lXi -lXtst
endif

all:
	$(MKDIR) build
	$(MKDIR) bin
	$(CC) -aI$(SRCDIR)/qweyboard -aI$(SRCDIR)/backends -aI$(SRCDIR)/backends/$(PLATFORM) $(CFLAGS) $(LANGVARIANT) -o $(BINDIR)/$(BIN) $(SRCDIR)/main -largs $(LIBS)

clean:
	$(RM) $(BUILDDIR)/*.o $(BUILDDIR)/*.ali $(BINDIR)/$(BIN)
