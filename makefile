CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc


all: core.cmo parser.cmi parser.cmo lexer.cmo query.cmo
	ocamlc -o query core.cmo lexer.cmo parser.cmo query.cmo
clean:
	rm *.cmo *.cmi

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo

.mll.mli:
	$(CAMLLEX) $<
.mll.ml:
	$(CAMLLEX) $<
.mly.mli:
	$(CAMLYACC) $<
.mly.ml:
	$(CAMLYACC) $<
.mli.cmi:
	$(CAMLC) -c $(FLAGS) $<
.ml.cmo:
	$(CAMLC) -c $(FLAGS) $<