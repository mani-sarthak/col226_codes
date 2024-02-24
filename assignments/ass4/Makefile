ass:
	ocamlc -c types.mli
	ocamlc -c types.ml 
	ocamlyacc parser.mly
	ocamlc -c parser.mli 
	ocamlc -c parser.ml 
	ocamllex lexer.mll
	ocamlc -c lexer.ml 
	ocamlc -c main.ml  
	ocamlc -o my_parser types.cmo parser.cmo lexer.cmo main.cmo