<p align="center">
  <img width="180" src="https://github.com/stacky-language/stacky/blob/main/docs/images/stacky.svg" alt="Stacky logo">
</p>

# Stacky
 Stacky is a simple stack-oriented programming language

## 概要

Stackyはスタックマシンの学習目的で設計された、スタック指向のシンプルなプログラミング言語です。一般的なスタックマシンに基づく明快な構文を備え、実際にコードを書きながらその動作原理を学ぶことができるようになっています。

## Getting Started

### インストール

Web上で動作する[Stacky Playground](https://stacky-language.github.io/playground)から簡単に試すことができます。

また、REPLや`.stacky`ファイルを実行するためのCLIツールをcargoからインストールできます。

```bash
$ cargo install stacky-cli
```

### Hello, World

まずは以下のコードを実行してみましょう。

```stacky
; hello.stacky
push "hello, world!"
println
```

Stacky Playgroundの場合はRunボタンを押してコードを実行します。CLIの場合は以下のコマンドを実行します。

```stacky
stacky hello.stacky
```

コードを実行すると`hello, world!`という文字列が出力されるはずです。

### FizzBuzz

Stacky言語はさまざまな命令をサポートしています。もう少し複雑な例を見てみましょう。

```stacky
; let i = 0
push 0
store i

start:

; i++
load i
push 1
add
store i

; if i == 100 return
load i
push 100
eq
br end

; if i % 15 == 0
load i
push 15
mod
push 0
eq
br fizz_buzz

; if i % 3 == 0
load i
push 3
mod
push 0
eq
br fizz

; if i % 5 == 0
load i
push 5
mod
push 0
eq
br buzz

goto other

fizz_buzz:
    println "fizz buzz"
    goto start

fizz:
    println "fizz"
    goto start

buzz:
    println "buzz"
    goto start

other:
    load i
    println
    goto start

end:
```

### ドキュメント

より詳細なドキュメントは[こちら](https://stacky-language.github.io/)で確認できます。

## ライセンス

このリポジトリは[MIT License](./LICENSE)の下で公開されています。