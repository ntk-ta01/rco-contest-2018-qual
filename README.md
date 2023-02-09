# 第2回 RCO日本橋ハーフマラソン 予選バチャ（皆解会004）

入力生成で用いられるGeneratorなどは、[qual_Aディレクトリ](https://github.com/recruit-communications/rco-contest-2018/tree/master/qual_A)のものを用います．

したがって、

```
.
├── qual_A
│   ├── tester
│   └── visualizer
├── src
│   └── bin
├── Cargo.lock
├── Cargo.toml
├── README.md
└── tools
    ├── Cargo.toml
    ├── in
    ├── out
    ├── seeds.txt
    └── src
```

のようにqual_Aディレクトリが含まれるようにします．qual_A内のjavaファイルはコンパイルしておきます．またRustの実行環境が存在することを期待します．

`seeds.txt`生成．

```bash
cargo run -p tools --bin seeds
```

入力生成．

```bash
cargo run -p tools --bin gen ./tools/seeds.txt --dir=./tools/in
```

テスト．全ケースACするような入出力だった場合，プログレスバーと合計点のみが表示されます．解法が1秒未満で終わるようなものだった場合，プログレスバーの描画がバグって97/100で終わることなどがありますが、たぶん全ケーステストしているはずです．

```
cargo run -p tools --bin tester
```

testerはデフォルトで`/src/bin/a.rs`を実行します．実行ファイルを変えたい場合、`/tools/src/bin/tester.rs`の57行目、58行目を書き換えてください．以下は，cppとpythonの例です．

```rust
// 実行部分
let p = std::process::Command::new("./a.out")
    // .args(["run", "--release"]) コメントアウトか削除する
```

```rust
// 実行部分
let p = std::process::Command::new("python3")
    .args(["a.py"])
```
