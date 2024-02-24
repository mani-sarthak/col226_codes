ass: parser.mly lexer.mll
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c types.mli types.ml
	ocamlc -o ass types.cmo parser.mli parser.ml lexer.ml main.ml
	./ass < input.txt


ass2:
	ocamlc -c types.mli types.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lexer.ml
	ocamlc -c main.ml
	ocamlc -o ass types.cmo parser.cmo lexer.cmo main.cmo
	./ass < input.txt
