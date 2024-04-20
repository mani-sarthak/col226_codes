ass: main.ml parser.mly types.ml lexer.mll input.pl
	ocamlc -c types.ml 
	ocamlyacc -v parser.mly
	ocamlc -c parser.mli 
	ocamlc -c parser.ml 
	ocamllex lexer.mll
	ocamlc -c lexer.ml 
	ocamlc -c main.ml  
	ocamlfind ocamlc -linkpkg -package unix -o ass types.cmo parser.cmo lexer.cmo main.cmo



clean:
	-rm *.cmi *.cmo output.txt lexer.ml ass parser.ml parser.mli tokens.txt parser.output
	-rm -rf out