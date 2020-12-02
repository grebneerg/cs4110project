MAIN := lang
OBJS := ast.cmo parser.cmo lexer.cmo pprint.cmo eval.cmo check.cmo main.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

$(MAIN): $(OBJS)
	ocamlc -o $@ $^

lexer.ml: lexer.mll
	ocamllex -q $<

parser.ml: parser.mly
	ocamlyacc -q $<

parser.mli: parser.mly
	ocamlyacc -q $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)

ast.cmo :
pprint.cmo : ast.cmo
eval.cmo : ast.cmo pprint.cmo
check.cmo : ast.cmo pprint.cmo
lexer.cmo : parser.cmi
main.cmo : parser.cmi lexer.cmo eval.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmi : ast.cmo
pprint.cmo : ast.cmo