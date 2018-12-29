FROM ubuntu:16.04

RUN set -eux; \
  sed -i.bak -e \
    "s%http://archive.ubuntu.com/ubuntu/%http://ftp.jaist.ac.jp/pub/Linux/ubuntu/%g" \
    /etc/apt/sources.list \
  ; \
  apt-get update; \
  DEBIAN_FRONTEND=noninteractive apt-get upgrade -y; \
  DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
    software-properties-common \
    curl git less make openssh-client patch perl rsync unzip xauth zip \
  ; \
  # opam
  curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh \
    | bash -x

RUN set -eux; \
  # DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
  DEBIAN_FRONTEND=noninteractive apt-get install -y \
    gcc m4 pkg-config libffi-dev libgmp-dev \
  ; \
  apt-get clean; \
  opam init -a -y --disable-sandboxing; \
  eval "$(opam env)"

RUN set -eux; \
  apt-key adv --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xF1656F24C74CD1D8; \
  add-apt-repository 'deb [arch=amd64,arm64,i386,ppc64el] http://ftp.yz.yamagata-u.ac.jp/pub/dbms/mariadb/repo/10.3/ubuntu xenial main'; \
  apt-get update; \
  DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
    libmariadb-dev \
    # libmariadbclient-dev \
    # libmysqlclient-dev \
    # inotify-tools \
  ; \
  apt-get clean