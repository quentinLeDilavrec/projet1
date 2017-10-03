build:
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/mstack.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/common.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/penrose_common.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/hanoi_common.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/penrose.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/hanoi.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/penrose_extended.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix hanoi_extended.cmi
	ocamlbuild -use-ocamlfind -pkg graphics -pkg unix src/main.byte

clean:
	ocamlbuild -clean

.PHONY: build clean