#!/bin/bash

set -eux

eval `opam env`

make deps
make rebuild

exec "$@"