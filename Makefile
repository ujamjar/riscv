all: lib sim

lib:
	ocamlbuild -use-ocamlfind riscv.cma

sim:
	ocamlbuild -use-ocamlfind sim.byte

tests:
	ocamlbuild -use-ocamlfind test_mem.byte test_instr.byte opcodes.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
