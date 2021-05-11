
TEST=test.jpl

all:
	dune build @all

run:
	dune exec ./jplc.exe -- $(TEST)

parse:
	dune exec ./jplc.exe -- -p $(TEST)
