MODULES=parser lexer 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

start:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean

zip:
	zip chipotlang.zip *.ml* *.md Makefile _tags .ocamlinit .merlin .gitignore *.guac