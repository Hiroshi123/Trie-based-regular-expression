
# Virtual Machine of regular expression

# Basic Idea

Primitive of regular expression can be deduced to following three operators.

1. Conjunction (concatenation)
2. Disjunction (alternation)
3. Repetition  (Kleene star)

By and large, their functionalities are shared with context-free-grammer in a sense.

For instance,

* Conjunction is a construction of compound statements.
* Disjunction is a construction of conditional statements.
* Repetion is a construction of iterates statements.

To let them execute on a turing machines, a building block of above three operations
must be partially modified.

To put its conclusion first,


|Opecode|1st Operand|2nd Operand|Description|
|:-|:-|:-|:-|
|char|character|check if an operand is matched with a given input character|
|split|program counter to be jumped||program counter to be jumped|jump 1st or 2nd|
|jump|program counter|decisively jump to program counter|
|match||this means input-sentense was consumed|

To map them, 

* Conjunction will be incrementation of program counter having one-character recognition before and after it.
  Let's call it `char` as mnemonic following the name of same functionality in parser combinator.

  e.g. `ab -> char a ; char b`

* Disjunction will be `split` instruction where two operands will be followed. The two operand
  indicates where next instructions heads. Generally, it would be preffered to have relative distance between
  the line number of the `split` instruction and both instruction which will be jumped.

  e.g. `(a|b) -> (line-nth) split n+1 n+3 ; char a ; jump (n+4) ; char b ; `

* Repetion will be mapped with `jump` & `split` as well as disjunction. The difference is `jump` instruction
  will always drive you one of previous split in case of representing repetition.

  e.g. `a* -> split (ln+1) (ln+3) ; char a ; jump n ; where ln = current line number`
  

# Bit code represenation of its vm

Since there are just 4 operands as primitives, the distinctions are described by 2bit.

On the other hand, bit length of each operands need to be considered.

As `char` instruction will hold any characters which can potentially be matched. the bit length of its operands
must be at least more than what was appeared on a given set of character.

If you will cover all of ASCII code, it needs 7 bit, but if your target is just `[a-zA-Z0-9_.] which is equivalent
to figures of base64 encoding, you just need 6 bit. This is good because `char` instruction needs just 8bit
(2 for opecode , 6 for a operand).

Assuming `char` instruction is withiin 8 bit, can you let other instructions in it as well?
You should take a pair of operands on `split` rather `jump` which just has 1 operands.

Since it has two operands, if 2 bit is consumed for selection of opecode, the half of left 6 bit which is 3 bit
is left for indication of following instruction.

For instance. if you have

`(abcdefgh|ig) `

3 bit is not enough to split from beginning of alternation to 2nd statement. 



