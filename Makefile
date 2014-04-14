SRC = jeu.ml main.ml

TO_COMPILE = $(SRC:.ml=.cmx)
TO_LINK = $(TO_COMPILE)

.PHONY : server clean

server : $(TO_COMPILE)
	ocamlopt $(TO_LINK) -o server

%.cmx : %.ml
	ocamlopt -c $^ -o $@

clean :
	rm -rf *.cmx