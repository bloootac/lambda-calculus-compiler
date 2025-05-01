# Lambdac - a compiler and runtime for lambda calculus

This project contains a compiler and runtime for a programming language heavily based on lambda calculus.

The runtime used here represents the program using SK combinatory logic; there is also [a runtime which uses lambda terms instead](https://github.com/AzureAether/lambda-runtime).

Please see [the project webpage](https://www.skyshoesmith.com/projects/lambda-language).

## Getting set up

### Stuff you'll need 
- [Alex](https://haskell-alex.readthedocs.io/en/latest/), a lexer generator for Haskell
- [Happy](https://haskell-happy.readthedocs.io/en/latest/), a parser generator for Haskell
- xxd, used to link an intermediate file to the runtime during compilation
	(Windows users: this comes with Git Bash, and can also be downloaded [here](https://sourceforge.net/projects/xxd-for-windows/)).
- [GHC](https://www.haskell.org/ghc), to generate the compiler
- [GCC](https://gcc.gnu.org/), to compile the runtime

### Build commands

In Bash:

```
chmod u+x lambdac
./lambdac build
```


## Language

### Syntax

A program can contain assignments and run statements. Assignments are used to define lambda terms, while run statements specify what lambda terms the runtime should reduce.

Single-line comments are denoted with the prefix `--`.

#### Assignments

Format: `[identifier] = [lambda_term]`

This allows the identifier to be used interchangeably with the lambda term in the rest of the program.
The identifier can contain any mixture of letters, digits, and the `_` and `'` symbols.

An identifier can be re-used several times in a program; when referenced in other statements, its most recent definition will be used. So the output of this program
```
t = \a b. a
run t f x
t = \a b. b
run t f x
```
will be
```
(f)
(x)
```

#### Lambda terms

Format:

```
[lambda_term] = \[variable list]. [lambda_term]
              | [lambda_term] [lambda_term] (application of two terms)
              | [variable]
```
A variable can contain a mixture of letters, digits, and the `_` and `'` symbols. 

Free variables can also be introduced in the program. For example, `\a. x a` is a valid lambda term; if `x` wasn't defined earlier in the program, it will be interpreted as a free variable.

The application of lambda terms is left-associative. Terms must be separated by a space.

The compiler parses `true` and `false` as `\a b.a` and `\a b.b` respectively. It also recognises integers as their Church numerals - for example, `3` will be interpreted as `\f x. f (f (f x))`.


#### Run statements

Format: `run [lambda_term]`

A program can contain multiple run statements. The runtime will reduce each of them in order and output the results. 

The compiler encodes a program into an SK combinator tree before performing reductions. Therefore, if `run` is called on an ungrounded function, the output will contain S and K combinators.

For example, the output of this program
```
run 0
run 1
run 2
```
will be
```
+K++SKK
++S++S+KSK+K++SKK
++S++S+KSK++S++S+KSK+K++SKK
```

### Example program
This program outputs the first five Fibonacci numbers.
```
-- Fibonacci program

isZero = \m. m (true false) true

tup = \a x b. b a x 

subOne = \n f x. (n (\t. tup 1 ((\x. (isZero (t true)) x (f x)) (t false))) (tup 0 x)) false

addOne = \m f x.f (m f x)
add = \m n. m addOne n

Y = \f. (\x. f(x x))(\x. f(x x))
fib = Y(\f i. isZero i 1 ((isZero (subOne i)) 1 (add (f (subOne i)) (f (2 subOne i)))))

run fib 0 f x
run fib 1 f x
run fib 2 f x
run fib 3 f x
run fib 4 f x
```

## Example use of the CLI 
```
$ ./lambdac
Welcome to the lambda compiler command line tool!
Try running 'help' to see a list of commands. Please run the 'build' command before first use.
>>> help
help                                        list all commands
build                                       generate the lexer, parser, and compiler
interpret [sourceFile] [targetFile]         store the combinatory representation of [sourceFile] at [targetFile]
compile [sourceFile] [targetFile]           compile [sourceFile] into an executable program stored at [targetFile]
run [sourceFile]                            compile [sourceFile] and run the program
quit                                        exit command line tool

>>> run program.txt
Compilation successful. Running program...
+(f)(x)
+(f)(x)
+(f)+(f)(x)
+(f)+(f)+(f)(x)
+(f)+(f)+(f)+(f)+(f)(x)
```

The tool will also accept a command given when calling the script:

```
$ ./lambdac run program.txt
Compilation successful. Running program...
+(f)(x)
+(f)(x)
+(f)+(f)(x)
+(f)+(f)+(f)(x)
+(f)+(f)+(f)+(f)+(f)(x)
```

## Features yet to be implemented 
- I/O combinators: the runtime could print out the current program tree when reducing an output combinator, and take in user input when reducing an input combinator
- Type checking: use the principal type algorithm to decide typability, and only reduce typable subtrees in a program