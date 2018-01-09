

## sed vm
 ( stream editor on a virtual machine )

Why does stream editor have to be run on a virtual machine?

This is just because things surrounding "sed" would be more
structually well understood.


First, when I started my implementation of stream-editor to empower my regular
expression module, I have just followed as what is described on its man page.

Namely, I just set typical states just like below,


```

data State_ = State_
  {
    --CONVFMT
    --convfmt :: [Char],
    set_ln    :: LineNumber,
    left_text :: Bs, 
    set_ps    :: PatternSpace,
    set_hs    :: HoldSpace,
    stdout    :: Bs,
    commands  :: [(Address,State State_ ())],
    options   :: Option
    
  } deriving (Show)

type LineNumber = Int

type Address =
  PatternSpace -> LineNumber -> Bool

data Option =
  Option'
  {
    line_output    :: Bool,
    -- which is set by -n ,--quiet or --silent option
    commandFile    :: [Bs],
    -- which is set by -f option
    inputFileName  :: [Bs],
    -- which is provided by standard command line argument 
    outputFileName :: [Bs],
    -- which is set by -a option, note without -a option,
    -- sed w function will create files while it executes 1st line
    kindReg        :: RegKind
    -- which is set by -E or -r
  }


```

Command is a tuple which contains "address" & "sed function", and sed function is
just a "state monad" which updates a state to cover itself.

Before upplying a set of each commands in each line, a current line will be given to
pattern space & will be streamed to stdout if nothing special happened.

By inserting some functions in between, you can substitue them, delete them, insert another
texts from a file, etc.

That is just another way of implementing them.

The drawback of this way or something which are not as satisfactory to me is
states are going to be huge if the succession of input files which was provided are large.
That does not sound like a very dashing stream.

That being said, sed has some functions which allows you to come back to the label that you have
set ever on a line of input file, such as `b :label t :label`, which make throwing ever read texts away
difficult.


After I started creating virtual machine for regular expression, I have started thinking what if I
let its own virtual machine code for sed, how much extent it would be nice.


## notes

question to myself

Q. is that a stack or register machine?

A. There is no nested structure but recognition of regular expression, suggesting it should be on
   a register machine.

Q. How many register do you need?

A. Minimum should be just 1 register which contains 1 bit.

Q. What is Basic instruction cycle and how many minimum opecode do you need ?

A. Most basic cycle is load & store.

```
1. load
2. store
```

You just load some bits no matter how many which is allowed from given register set and put
it in a register which is called patten space in sed , and
store it in a memory which is buffered stdout in general.
If there is no functions are set, just 2 opecode is enough.

If you have a function, then you need to insert following 3 just like typical instruction cycle would have.

```
1. load
2. fetch   <- fetching a next instruction
3. decode  <- recognizing opecode & operand and execute
4. execute <- running 
5. store
```

If you have enough registers which enables you to put all of instructions which represent
pairs of address & function that you ought to apply to given files, you do not need
(fetch->decode->execute) but only (execute)
as you can place these instructions on some registers till the end of
a given input file.


Normally, a file you received has multiple lines, besides a line contains many characters.
But, it is assumed number of registers and bits that these registers can hold is limited.
That means, sequence of instruction is going to be long unless you set cmp & br & jump instruction
just like below.

```
1. cmp 
2. br (bool) 3 7
3. load
4. (fetch-decode-execute)
5. store
6. jump 1
7. end
```

If you make a distinction between a cycle for character inputs from a line and a cycle for a line from a file,
you need to set anohter br with instruction to increment line number.

```
1. cmp  <- if a line is EOF or not 
2. br 3 10 
3. inc <- increment line number
4. cmp <- if a character is \n
5. br 4 1
6. load
7. (fetch-decode-execute)
8. store
9. jump 5
10. end
```

Q. how many opecode realistically do you need?

A. The answer to this question needs to be long.

Let us think one by one refferring possible sed function.

### Address

But, before anything else, do not forget all of sed commands may have address which works
as a filter function before applying following function to pattern space.

Note that if address provides true on a given input, texts on a pattern space is going to be streamed
to stdout as they are.

To represent this function on VM, you need br instruction where you will step next or jump over the instruction
which corresponds to a following instruction.

```
1. cmp 
2. br (Bool) 3 4
3. an instruction
4. store
```

Sed address might be either set of numbers or context address which contains regular expression.

To filter with line number, one of the register should be spent to hold sort of program counter
so that you can compare given address and current line number. And you need 1 additional opecode which
is cmp arg1 arg2 where both arg1 and arg2 should be integer to represent a line number.

What about cases where context address which holds regular expression comes?

If you have already read the page about vm about regular expression,
you must know what's needs to be done is
adding instruction for regular expression.
These instruction provides bool value to indicate if regular expression is matched or not which tells next br go
to an instruction and apply a function or jump it.


### function

Most famous and representative function among all sed functions must be `s`
; the abbreviation of "substitution" which has two parameters.

where you just load from std


And, following is a note about its opecode.

|opcode|description|
|:-|:-|

|fetch|fetch instruction from|
|decode||
|load|load some bits from a current line|
|loadf|
|store|store to stdout which is buffered meanwhile till the end of all exectution|
|storef||
|move||
|jump||
|inc||


#### Register
