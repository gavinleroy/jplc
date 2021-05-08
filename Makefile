
TEST=test.jpl

all:
	dune build @all

run:
	dune exec ./jplc.exe -- $(TEST)

test:
	dune test
