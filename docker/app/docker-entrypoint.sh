#!/bin/bash

set -eux

eval `opam env`

make deps
make rebuild

# Wait for db
dockerize \
  -wait tcp://${ISUBATA_DB_HOST}:${ISUBATA_DB_PORT} \
  -wait-retry-interval 5s \
  -timeout 2m \
  "$@"
