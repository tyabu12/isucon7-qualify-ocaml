#!/bin/bash

set -eux

# Wait fo app
dockerize \
  -wait http://${ISUBATA_APP_HOST}:${ISUBATA_APP_PORT} \
  -wait-retry-interval 15s \
  -timeout 10m \
  "$@"
