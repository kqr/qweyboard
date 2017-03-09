PLATFORM=x11
# Comment out to use the dummy platform instead
# PLATFORM=dummy_platform

ifeq ($(PLATFORM),x11)
LIBS=-lX11 -lXi -lXtst
endif

all:
	gprbuild -p

clean:
	gprclean
