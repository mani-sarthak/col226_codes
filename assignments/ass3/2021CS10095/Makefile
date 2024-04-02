# write testcases in the fie itself
script: 2021CS10095_A3.ml
	ocaml str.cma 2021CS10095_A3.ml > out.txt

lex: lexer.l
	lex lexer.l
	gcc -o lexer lex.yy.c
	./lexer < check.txt > out.txt

ocamllex: ocaml.mll
	ocamllex ocaml.mll

c: main.ml new.mll
	ocaml new.mll
	ocamlc -o c main.ml new.ml
	./c  check.txt  out.txt
	

clean: 
	rm -f lexer lex.yy.c out.txt