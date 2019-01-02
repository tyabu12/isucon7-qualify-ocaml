include .env
export $(shell sed 's/=.*//' .env)

.DEFAULT_GOAL := build

.PHONY: deps build rebuild exec test clean

gen-initial-dataset:
	docker run --name=isu7q-$@ golang:1.9 bash -c \
		"go get github.com/constabulary/gb/... && \
		git clone --depth 1 https://github.com/isucon/isucon7-qualify.git && \
		cd isucon7-qualify/bench && \
		gb vendor restore	&& \
		make && \
		./bin/gen-initial-dataset"
	docker cp isu7q-$@:/go/isucon7-qualify/bench/isucon7q-initial-dataset.sql.gz ./db
	docker rm isu7q-$@

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
	dune exec isucon7-qualify-ocaml

test:
	dune runtest

clean:
	dune clean