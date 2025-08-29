<p align="center">
  <img width="180" src="https://github.com/stacky-language/stacky/docs/images/stacky.svg" alt="Stacky logo">
</p>

# Stacky
Stacky is a simple stack-oriented programming language.

## Overview

Stacky is a stack-oriented, minimal programming language designed for learning how stack machines work. It provides a clear syntax based on common stack machine concepts so you can learn the execution model by writing and running code.

## Getting Started

### Installation

You can try Stacky quickly using the web-based [Stacky Playground](https://stacky-language.github.io/playground):

You can also install a CLI tool to run a REPL or execute `.stacky` files via cargo:

```bash
$ cargo install stacky-cli
```

### Hello, World

Run the following example:

```stacky
; hello.stacky
push "hello, world!"
print
```

In the Stacky Playground, press the Run button to execute the code. From the CLI, run:

```bash
stacky hello.stacky
```

The program should print `hello, world!`.

### FizzBuzz

Stacky supports a variety of instructions. Here's a slightly more complex example:

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
    print "fizz buzz"
    goto start

fizz:
    print "fizz"
    goto start

buzz:
    print "buzz"
    goto start

other:
    load i
    print
    goto start

end:
```

### Documentation

For more detailed documentation, see: https://stacky-language.github.io/

## License

This repository is under the [MIT License](./LICENSE).
