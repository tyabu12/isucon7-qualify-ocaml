FROM ubuntu:16.04

ENV OPAM_INSTALL_SH_URL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh

ENV DOCKERIZE_VERSION v0.6.1
ENV DOCKERIZE_URL https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz

# Set up opam
RUN set -eux; \
  : "Install dependencies to install opam"; \
  sed -i.bak -e \
    "s%http://archive.ubuntu.com/ubuntu/%http://ftp.jaist.ac.jp/pub/Linux/ubuntu/%g" \
    /etc/apt/sources.list \
  ; \
  apt-get update; \
  # DEBIAN_FRONTEND=noninteractive apt-get upgrade -y; \
  DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
    software-properties-common \
    curl git less make openssh-client patch perl rsync unzip xauth zip \
  ; \
  : "Install opam"; \
  curl -sSL $OPAM_INSTALL_SH_URL | bash -x; \
  : "Install dependencies for opam init"; \
  # DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
    gcc m4 pkg-config libffi-dev libgmp-dev \
  ; \
  : "Initialize opam"; \
  opam init -a -y --disable-sandboxing; \
  eval "$(opam env)"

# Install application dependencies
RUN set -eux; \
  : "Install dockerize"; \
  curl -sSL $DOCKERIZE_URL | tar xz -C /usr/local/bin; \
  : "Install database libirary"; \
  apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xF1656F24C74CD1D8; \
  add-apt-repository 'deb [arch=amd64,arm64,i386,ppc64el] http://ftp.yz.yamagata-u.ac.jp/pub/dbms/mariadb/repo/10.3/ubuntu xenial main'; \
  apt-get update; \
  DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
    libmariadb-dev \
    # libmariadbclient-dev \
    # libmysqlclient-dev \
    # inotify-tools \
  ; \
  : "Clean up"; \
  apt-get clean; \
  rm -rf /var/lib/apt/lists/*

COPY docker-entrypoint.sh /

ENTRYPOINT ["/docker-entrypoint.sh"]