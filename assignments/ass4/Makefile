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
	./my_parser < input.txt > output.txt


clean:
	rm *.cmi *.cmo output.txt lexer.ml my_parser parser.ml parser.mli