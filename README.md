# ISUCON7 予選問題の OCaml 参照実装 (非公式)

OCaml の遊び用。

## 必要要件

- git
- make
- [opam](https://opam.ocaml.org/doc/Install.html) >= 2.0

## 実行方法

```sh
# 取得
git clone git@github.com:tyabu12/isucon7-qualify-ocaml.git
cd isucon7-qualify-ocaml

# 依存ライブラリ群のインストール
opam update
# opium エラー回避 (https://github.com/rgrinberg/opium/issues/84)
opam pin add -y --dev-repo opium
opam pin add -yn isucon7-qualify-ocaml . &&
opam install --deps-only isucon7-qualify-ocaml

# ビルド
make

# 実行
make exec
```

## 謝辞

下記のデータは[本家](https://github.com/isucon/isucon7-qualify)のものを使用しています。

- public/
  - [isucon7-qualify/webapp/public](https://github.com/isucon/isucon7-qualify/tree/master/webapp/public)
- db/isubata.sql
  - [isucon7-qualify/db/isubata.sql](https://github.com/isucon/isucon7-qualify/blob/master/db/isubata.sql)
