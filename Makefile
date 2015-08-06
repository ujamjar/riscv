.PHONY: clean all install uninstall 

all: setup.data
	ocaml setup.ml -build

setup.ml:
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove riscv

clean:
	ocaml setup.ml -clean
	- find . -name "*~" | xargs rm

distclean:
	ocaml setup.ml -distclean

#all: lib tests
#
#lib:
#	ocamlbuild -use-ocamlfind riscv.cma riscv.cmxa
#
#tests:
#	ocamlbuild -use-ocamlfind test_mem.byte test_instr.byte test_decoder.native test_elf.byte
#
#opcodes:
#	ocamlbuild -use-ocamlfind genops.byte
#	./genops.byte
#
#install:
#	ocamlfind install riscv src/META \
#		_build/src/*.mli \
#		_build/src/riscv.cma \
#		_build/src/riscv.cmxa \
#		_build/src/riscv.a 
#
#uninstall:
#	ocamlfind remove riscv
#
#clean:
#	ocamlbuild -clean
#	-find . -name "*~" | xargs rm -f
#	-rm -f *.byte
#	-rm -f *.native
