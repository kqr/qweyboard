
CC=gnatmake
SRCDIR=src
BUILDDIR=build
BINDIR=bin
LANGVARIANT=-gnatW8
CFLAGS=-D $(BUILDDIR)
LIBS=-lX11 -lXi -lXtst

all:
	$(CC) $(CFLAGS) $(LANGVARIANT) -o $(BINDIR)/qweyboard src/main -largs $(LIBS)

clean:
	$(RM) $(BUILDDIR)/*.o $(BUILDDIR)/*.ali $(BINDIR)/main
