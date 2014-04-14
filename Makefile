SRC = jeu.ml main.ml

DEP = unix.cmxa

TO_COMPILE = $(SRC:.ml=.cmx)
TO_LINK = $(DEP) $(TO_COMPILE)

.PHONY : server clean

server : $(TO_COMPILE)
	ocamlopt $(TO_LINK) -o server

%.cmx : %.ml
	ocamlopt -c $^ -o $@

clean :
	rm -rf *.cmx