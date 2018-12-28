.DEFAULT_GOAL := build

.PHONY: build test clean

build:
	dune build

exec:
	dune exec ./app.exe

# test:
# 	dune runtest

clean:
	dune clean