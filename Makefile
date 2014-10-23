all: lib tests

lib:
	ocamlbuild -use-ocamlfind riscv.cma riscv.cmxa

tests:
	ocamlbuild -use-ocamlfind test_mem.byte test_instr.byte test_decoder.native test_elf.byte

opcodes:
	ocamlbuild -use-ocamlfind genops.byte
	./genops.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
	-rm -f *.byte
	-rm -f *.native
