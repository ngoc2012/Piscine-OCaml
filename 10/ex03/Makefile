all:
	rm -f *.cmo *.cmi *.cmx *.o main
	ocamlopt -c Deck.mli
	ocamlopt -c Deck.ml
	ocamlopt -c main.ml
	ocamlopt -o main Deck.cmx main.cmx
	./main
