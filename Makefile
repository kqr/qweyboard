ifeq ($(shell uname),Linux)
PLATFORM=x11
else
$(error Linux is currently the only supported platform)
endif

BIN=qweyboard
BINDIR=bin
SRCDIR=src
BUILDDIR=build

CC=gnatmake
LANGVARIANT=-gnatW8
CFLAGS=-D $(BUILDDIR)

ifeq ($(PLATFORM),x11)
LIBS=-lX11 -lXi -lXtst
else
LIBS=
endif

all:
	$(CC) -aI$(SRCDIR)/$(PLATFORM) $(CFLAGS) $(LANGVARIANT) -o $(BINDIR)/$(BIN) $(SRCDIR)/main -largs $(LIBS)

clean:
	$(RM) $(BUILDDIR)/*.o $(BUILDDIR)/*.ali $(BINDIR)/$(BIN)
