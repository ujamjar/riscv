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

