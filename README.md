# ISUCON7 予選問題の OCaml 参照実装 (非公式)

[![CircleCI](https://circleci.com/gh/tyabu12/isucon7-qualify-ocaml.svg?style=svg)](https://circleci.com/gh/tyabu12/isucon7-qualify-ocaml)

OCaml の遊び用。

## 必要要件

- git
- make
- [opam](https://opam.ocaml.org/doc/Install.html) >= 2.0

## 実行方法

### Docker

[docker-compose](https://docs.docker.com/compose/install/) が必要です。

```sh
# 環境変数のコピー
cp .env.example .env

# 以下は初回のみ、コンテナのビルドが走るので時間がかかります

# アプリケーションコンテナとDBコンテナを起動
docker-compose up -d app db

# ベンチを投げる
docker-compose run bench

# アプリケーションのエラーログの確認
docker-compose logs app

# アクセス
curl http://localhost
```

### ローカル

実行にはDBが必要です。MySQL 5.7 を推奨。

```sh
# 依存ライブラリ群のインストール
make deps

# ビルド
make

# DBの環境変数の設定
cp .env.example .env
$EDITOR .env

# 実行
make exec

# アクセス
curl http://localhost
```

## 謝辞

下記のデータは[本家](https://github.com/isucon/isucon7-qualify)のものを使用しています。

- public/
  - [isucon7-qualify/webapp/public](https://github.com/isucon/isucon7-qualify/tree/master/webapp/public)
- db/isubata.sql
  - [isucon7-qualify/db/isubata.sql](https://github.com/isucon/isucon7-qualify/blob/master/db/isubata.sql)
