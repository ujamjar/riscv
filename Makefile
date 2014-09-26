all: lib sim

lib:
	ocamlbuild -use-ocamlfind riscv.cma

sim:
	ocamlbuild -use-ocamlfind sim.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f

