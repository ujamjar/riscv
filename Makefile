all: lib tests

lib:
	ocamlbuild -use-ocamlfind riscv.cma riscv.cmxa

tests:
	ocamlbuild -use-ocamlfind test_mem.byte test_instr.byte test_decoder.native test_elf.byte

opcodes:
	ocamlbuild -use-ocamlfind genops.byte
	./genops.byte

install:
	ocamlfind install riscv src/META \
		_build/src/*.mli \
		_build/src/riscv.cma \
		_build/src/riscv.cmxa \
		_build/src/riscv.a 

uninstall:
	ocamlfind remove riscv

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
	-rm -f *.byte
	-rm -f *.native
