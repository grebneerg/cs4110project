OBJS := ast.cmo eval.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

lexer.ml: lexer.mll
	ocamllex -q $<

parser.ml: parser.mly
	ocamlyacc -q $<

parser.mli: parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi $(MAIN)