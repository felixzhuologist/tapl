# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

.PHONY: all clean byte native profile debu sanity test

OCB_FLAGS = -use-ocamlfind -use-menhir -pkg core -I src -I lib
OCB = corebuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

# test:
	# native
	# ./main.native "2 + 3 * 3"