all: eqprover

eqprover: lang.cmo parser.cmo lexer.cmo interf.cmo subst.cmo unif.cmo rewriting.cmo eqprover.cmo
	ocamlc -o eqprover $^


# Compilation of .ml files
eqprover.cmo: eqprover.ml lang.cmo interf.cmo rewriting.cmo
	ocamlc -c $<

lang.cmo: lang.ml 
	ocamlc -c $<

interf.cmo: interf.ml lexer.cmo
	ocamlc -c $<

subst.cmo: subst.ml lang.cmo
	ocamlc -c $<

unif.cmo: unif.ml subst.cmo
	ocamlc -c $<

rewriting.cmo: rewriting.ml unif.cmo interf.cmo
	ocamlc -c $<

##### ocamlyacc parser

# ocaml lexer and parser
lexer.ml: lexer.mll lang.cmo
	ocamllex $<

parser.ml parser.mli: parser.mly lang.cmo
	ocamlyacc -v $<

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c $<
parser.cmo: parser.ml parser.cmi lang.cmo
	ocamlc -c $<

#### Generic rules

%.cmi: %.mli
	ocamlc -c $<


.PHONY: clean

clean: 
	rm lexer.ml parser.ml *.mli *.cmi *.cmo
