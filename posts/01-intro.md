
# IceForth for the Nintendo Entertainment System

IceForth is a language for writing programs on the NES, hosted on a virtual NES
cartridge. A few kb of bootstrap machine code is loaded in a virtual 6502
processor, enough to bootstrap Forth, including an assembler and disassembler.
The cartridge behaves like a NES cartridge with writeable memory where there
would usually be ROM. As it reads the source code, it compiles and extends itself,
filling the cartridge with the rest of the Forth environment and the NES program.
When the source executes the word `freeze`, it freezes everything into a tidy ROM
file and stops the simulation. This ROM file contains all the information it needs
to pick up right where freeze left off, returning from `freeze` as if nothing nothing
happened and keeps going.

## The simulator

The simulator is a very basic 6502 simulator. It loads the 64kb file bootstrap.bin,
which is the output of the assembler. It simulates the NES memory map, where
00-$7FF is RAM, and $4020-$FFFF is under the control of the cartridge. It adds one
thing to the hardware -- an io port at an address that was part of the test mode
of the real processor. This memory address reads

## The assembly bootstrap


## Understanding Forth

There are many resources online explaining how the Forth
language works. (LINKS)
The most important parts of the Forth interpreter are the
stacks. These are the primary way of passing data around.
In the example,

```
1 2 +
```

`1` pushes the value one onto the data stack, `2` pushes the value two
onto the data stack, and `+` takes the top two values from the data stack, adds
them, and pushes the result back onto the data stack. This notation, of
operands followed by operator is known as reverse Polish notation.

Forth also has a return stack. This stack is used to store the call stack,
so that words can call other words and return back to just after the call.

To oversimplify, here's how the Forth interpreter works:
First, read input characters until you find a space. Each space-delimited
string is known as a word. `dup`, `1+`, `'`, `2`, etc. are all valid word names.
First, the interpreter looks up the word in the dictionary. If it's not found,
then it tries to parse it as a number. If those fail, then show an error.

Now, the interpreter checks the value of its `state` variable. If the state
is interpreting, then it will immediately execute the word. For example, if the word is
`!` (store), then it will immediately read the address and value from the stack
and write the value to the address. Numbers are executed by pushing them onto the
stack.

If the state is compiling, then it will compile the word. For regular words,
this means appending `jsr CodeForWord` to the code area. For numbers, this
means appending assembly code to push the value to the code area.

Now this is a very simplified view. The first thing that modifies this basic loop
are the flags in the dictionary.

- immediate: If a word has this flag, then it will always execute immediately,
  even in compile mode. For example, the word `[` simply switches `state` to
  interpret mode. It must do this while in compile mode, otherwise there would be
  no way to switch out of compile mode, so it is marked with the
  immediate flag.
- always\_inline: Instead of compiling as `jsr CodeForWord`, when in compile mode
  these words are inlined. This saves the overhead of the call. For example, the
  word drop is implemented as:

  ```
  drop:
    inx
    inx
    rts
  ```

  Inlined, drop is 4 cycles, and 2 bytes, but with the subroutine it is 19 cycles
  and 3 bytes. It's always a good idea to inline it.

- hidden: A word with this flag is in the dictionary but will not be found during
  a dictionary search.

Besides the flags, as words are executing they can interact with the words used for
interpretation. I've already mentioned `[` which modifies the state variable (it also has 
a twin `]`). The input stream is also open. Though the interpreter only cares about
space delimited strings, a word is free to consume characters from the input stream
before returning to the interpreter. The word `."` prints a string, and is used like
this:

```
." Hello world"
```

The interpreter sees the word `."`, which is marked immediate, so it executes it.
`."` then consumes characters until it sees the close quote mark, and either executes
or compiles code to print the string, before returning to the interpreter. This allows
new words to easily extend the interpreter.

## Understanding the Nintendo Entertainment System

lda #0
sta Stack, x
lda #0
sta Stack+1, x
