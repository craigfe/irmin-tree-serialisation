all:
	dune build

test:
	dune exec -- ./src/main.exe &>k
