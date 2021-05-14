
TEST=test.jpl

all:
	dune build @all

run:
	dune exec ./jplc.exe -- $(TEST)

parse:
	dune exec ./jplc.exe -- -p $(TEST)

type:
	dune exec ./jplc.exe -- -t $(TEST)

