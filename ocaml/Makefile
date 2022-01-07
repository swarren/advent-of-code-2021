srcs := $(wildcard *.ml)
bins := $(srcs:.ml=.bin)

all: $(bins)

%.bin: %.ml Makefile
	ocamlopt.opt -o $@ $<
