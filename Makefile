SRC = matrices.ml grappe.ml assignement.ml correspondances.ml
INTERFACES = matrices.mli grappe.mli assignement.mli correspondances.mli

A_COMPILER_BC = $(INTERFACES:.mli=.cmi) $(SRC:.ml=.cmo)
A_COMPILER_NAT= $(INTERFACES:.mli=.cmi) $(SRC:.ml=.cmx)
A_LINKER_BC = $(SRC:.ml=.cmo)
A_LINKER_NAT= $(SRC:.ml=.cmx)

EXEC = dissecter

.PHONY : default bytecode natif clean
default : bytecode
	echo "Par défaut, le compilateur est ocamlc. 'make natif' compilera en natif."
bytecode : $(A_COMPILER_BC)
	ocamlc $(A_LINKER_BC) -o $(EXEC)
natif : $(A_COMPILER_NATIF)
	ocamlopt $(A_LINKER_NATIF) -o $(EXEC)
clean :
	rm -rf *.cmo *.cmx *.cmi $(EXEC)
%.cmi : %.mli
	ocamlc -c $^ -o $@
%.cmo : %.ml
	ocamlc -c $^ -o $@
%.cmx : %.ml
	ocamlopt -c $^ -o $@