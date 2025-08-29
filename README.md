<p align="center">
  <img width="200" src="https://github.com/stacky-language/stacky/blob/main/docs/images/stacky.png" alt="Stacky logo">
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
println
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

## Reference

### Basic Syntax

Stacky parses code line by line. Each line can contain one command or label.

```stacky
push 1
push 2
add
println
```

Comments are written with `;`.

```stacky
; this is comment
push "comment"
println
```

### Value Types

Stacky values ​​have one of the following types:

* nil
* int
* float
* string
* bool

These values ​​can be pushed onto the stack using the `push` command.

```stacky
push nil
push 10
push 1.2
push "foo"
push true
```

### Variables

The `store`/`load` commands allow you to save values ​​to registers independent of the stack.

```stacky
; i = 2
push 2
store i

; println(i + 3)
load i
push 3
add
println
```

### Control Structure

You can transfer program control using labels and the `goto` command. Labels can be defined with `label:`.

```stacky
goto label

push "unreachable"
println

label:
```

You can also use the `br` command to transfer to a label only if the top of the stack is `true`.

```stacky
; a = 1
push 1
store a

; b = 2
push 2
store b

; a == b
load a
load b
eq

br then
goto else

then:
    println "a is equal to b"
    goto end
else:
    println "a is not equal to b"
    goto end
end:
```

### Inline arguments

Commands pop arguments from the stack by default.

```stacky
; println "hello"
push "hello"
println

; 1 + 2
push 1
push 2
add
```

If the arguments are constants, you can write them inline.

```stacky
push "hello"
add 1 2
```

## License

This repository is under the [MIT License](./LICENSE).
