# ISUCON7 予選問題の OCaml 参照実装 (非公式)

OCaml の遊び用。

## 必要要件

- git
- make
- [opam](https://opam.ocaml.org/doc/Install.html) >= 2.0

## 実行方法

実行にはDBが必要です。

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
curl http://localhost:3000
```

## 謝辞

下記のデータは[本家](https://github.com/isucon/isucon7-qualify)のものを使用しています。

- public/
  - [isucon7-qualify/webapp/public](https://github.com/isucon/isucon7-qualify/tree/master/webapp/public)
- db/isubata.sql
  - [isucon7-qualify/db/isubata.sql](https://github.com/isucon/isucon7-qualify/blob/master/db/isubata.sql)
