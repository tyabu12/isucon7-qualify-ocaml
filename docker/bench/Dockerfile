FROM golang:1.9

WORKDIR /usr/src/isucon7-qualify-bench

ENV DOCKERIZE_VERSION v0.6.1

RUN set -eux; \
  : "Build bench command"; \
  go get github.com/constabulary/gb/...; \
  git clone --depth 1 https://github.com/isucon/isucon7-qualify.git; \
  mv isucon7-qualify/bench/* .; \
  rm -rf isucon7-qualify; \
  gb vendor restore; \
  make; \
  : "Install dockerize"; \
  curl -sSL https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
    | tar xz -C /usr/local/bin; \
  : "Install jq to analyze bench result"; \
  apt-get update; \
  DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y jq; \
  : "Clean up"; \
  apt-get clean; \
  rm -rf /var/lib/apt/lists/*

COPY docker-entrypoint.sh /

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["bash", "-c", "./bin/bench -remotes=${ISUBATA_APP_HOST}:${ISUBATA_APP_PORT}"]