
TEST=src/test.jpl

EXEC=src/jplc.exe

all:
	cd src/
	dune build @all

run:
	cd src/
	dune exec $(EXEC) -- $(TEST)

parse:
	cd src/
	dune exec $(EXEC) -- --emit-parse $(TEST)

type:
	cd src/
	dune exec $(EXEC) -- --emit-type $(TEST)

flat:
	cd src/
	dune exec $(EXEC) -- --emit-flat $(TEST)

llvm:
	cd src/
	dune exec $(EXEC) -- --emit-llvm $(TEST)

clean:
	rm -r _build
