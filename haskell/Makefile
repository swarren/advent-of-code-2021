srcs := $(wildcard *.hs)
bins := $(srcs:.hs=.bin)

all: $(bins)

%.bin: %.hs Makefile
	ghc -O2 -o $@ $<
