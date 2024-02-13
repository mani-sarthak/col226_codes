simple_lexer: simple_lexer.l 
	lex simple_lexer.l 
	gcc -o simple_lexer lex.yy.c

test: simple_lexer
	./simple_lexer < check.ml > basic.txt

ocamlLex: ocaml.mll
	ocamllex ocaml.mll
	ocamlc -o check ocaml.ml check.ml
	./check > ocaml.txt