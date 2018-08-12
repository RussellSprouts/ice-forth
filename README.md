# Ice Forth

A Forth implementation self-hosted on the NES. Compiling a cartridge
has several stages -- first, the assembly code is compiled into an initial
cartridge. The initial cartridge has the basics of a Forth interpreter 
and some other goodies. `6502.lua` emulates a NES cartridge with RAM
instead of ROM, and the Forth sources are streamed in to the interpreter.
The sources can write in the cartridge memory and add new code. When
it's finished, the command `freeze` tells the emulator that the
cart is ready. The RAM of the cartridge is frozen into a ROM file and
saved in `out.nes`.

## Features

- Not really compliant with ANS Forth :/
- Subroutine-threaded compilation
- Includes assembler and disassembler for NMOS 6502.
- Alpha state -- very buggy

## Running

Edit `make.sh` with the path to your ca65 and ld65 executables.
Ensure you have luajit installed.

```
$ ./make.sh
$ luajit 6502.lua
```

Try some commands:

```
Welcome to Forth!
\ Math examples
1 2 + .

hex FF . decimal
```

```
Welcome to Forth!
\ Disassembler example
['] val >impl disas
```

```
Welcome to Forth!
\ Defining new words
: six 1 2 3 + + ;
```

```
Welcome to Forth!
\ Assembler usage
: oam-dma [
  hex
  sprites >byte lda.#
  4014          sta
  decimal
] ;
```

## Implementation details

### Dictionary format

A dictionary entry looks like this:

```
DUP:
jmp DUP_IMPL
.byte reserved
.byte name-length | Flags
.byte "DUP"
```

There is no previous pointer since the next entry comes right after
the name.

There are 5 + name-length bytes in each entry. The dictionary
grows downwards from the end of the address space, while the
actual code of the code starts near the start of the address
space and grows up. This separation is so that code can theoretically
be relocated. Eventually, words will be optimized by bringing them
to the front of the code area, shifting everything back, and then
modifying their code. The only thing that needs to change when a word
is relocated is to update the target of the jmp instruction.
(Assuming the word itself is written with relocatable code).

Since the same address is the entry point for the code and the
start of the dictionary entry, execution tokens and dictionary
entry pointers are the same.

### Subroutine threaded code

The code is compiled to subrouting-threaded code with many inlined
functions. Try `['] val >impl disas` to the see the disassembly
for the `val` word.
