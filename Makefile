all: sol

clean:
	rm -f *.cm*

distclean: clean
	rm -f sol *~

sol: main.ml sol.mli sol.cmo
	ocamlc -o sol sol.cmo main.ml

sol.cmo: sol.ml sol.mli
	ocamlc -c sol.mli
	ocamlc -c sol.ml
