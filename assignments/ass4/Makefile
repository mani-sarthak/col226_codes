ass: main.ml parser.mly types.mli types.ml lexer.mll input.pl
	ocamlc -c types.mli
	ocamlc -c types.ml 
	ocamlyacc parser.mly
	ocamlc -c parser.mli 
	ocamlc -c parser.ml 
	ocamllex lexer.mll
	ocamlc -c lexer.ml 
	ocamlc -c main.ml  
	ocamlc -o ass types.cmo parser.cmo lexer.cmo main.cmo
	./ass < input.pl > output.txt	
#	make test


test: ass input.pl
	@mkdir out
	for file in ./test/*.pl; do \
        ./ass < $$file > ./out/$$(basename $$file .pl); \
    done


clean:
	-rm *.cmi *.cmo output.txt lexer.ml ass parser.ml parser.mli tokens.txt 
	-rm -rf out