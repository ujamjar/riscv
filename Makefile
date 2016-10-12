.PHONY: clean all build

all: build

build:
	ocaml pkg/pkg.ml build

clean:
	ocamlbuild -clean
	- find . -name "*~" | xargs rm

gen:
	ocamlbuild genops.byte

