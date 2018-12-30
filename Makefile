.DEFAULT_GOAL := build

.PHONY: deps build rebuild exec test clean

deps:
	opam update
	# https://github.com/rgrinberg/opium/issues/84
	opam pin add -y --dev-repo opium
	opam pin add -yn .
	opam install -y --deps-only .

build:
	dune build

rebuild: clean build

exec:
	dune exec ./app.exe

# test:
# 	dune runtest

clean:
	dune clean