# Reg-puchi

## Motivation

Regular Expressions are so cute  (｀・ω・´ )/

That is why this repository is to my best effort dedicated for embracing them.

## Features

### Towards regular expression itself

* Automaton & Tree Visualization with GraphViz

* Supports from monadic parser combinator [docs](https://github.com/Hiroshi123/reg-puchi/blob/master/docs/regvspsc.md)

* NFA to NDF -> under maintanance

* Translation to vm code [docs](https://github.com/Hiroshi123/reg-puchi/blob/master/docs/regvm.md)

### Towards its application

I am making corresponding virtual machine and bit-code which fits capability of each commands with parsing them
from scratch. Take a look at documents below.

* awk :: [docs] ()

* sed  [docs] (https://github.com/Hiroshi123/reg-puchi/blob/master/docs/sedvm.md)

* grep :: [docs] ()

## Documentation

Rather than being called solid documentations,
there are some notes about my thoughts on this topic.

Check `./docs/` out.

## Example

* awk
* sed
* grep

### any reg-expression-heavily-dependent commands will be added.

## Reference Links

A Regular Expression Virtual Machine
https://swtch.com/~rsc/regexp/regexp2.html

Monadic Parser Combinator
http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf


http://sebfisch.github.io/haskell-regexp/regexp-play.pdf
