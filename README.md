# Ice Forth

A Forth implementation self-hosted on the 6502, for creating NES cartridges.

Compiling a cartridge has several stages.

1. The assembly code (.s files) are compiled into an initial cartridge
   `bootstrap.bin`. This is a 64kb file containing the full memory space at
   startup for the next step. In that space we have the the basics of a Forth
   interpreter with an integrated assembler. (This only takes up a few kb of the
   full 64).

2. `6502.lua` loads `bootstrap.bin` into its memory, and emulates running the
   interpreter on the NES processor, using a virtual cartridge with RAM everywhere.
   The emulator has the crucial addition of an IO port, which is used to input
   Forth code and get Forth output.

3. A minimal subset of the Forth environment is defined in assembly, so next we
   stream in the contents of `bootstrap.f` to the running cartridge. This defines 
   the rest of the base Forth environment.

4. Other Forth code is defined in other files which are streamed in, and afterward
   the REPL opens for the user to test words. This Forth code is compiled into what
   will become cartridge space on the NES, and defines the NES program. The emulation
   finishes at the execution of the word `freeze`.

5. `freeze` will turn the running interpreter into a ROM. The contents of NES RAM in the
   intepreter are compressed and moved into the cartridge space so they can be restored
   after freeze finishes, but the Forth data, return, and control flow stacks are
   emptied. The Forth internal temp locations are also emptied. Once the compressed data
   is written, the memory space from $8000-$FFFF is now permanently frozen. This section
   of memory becomes the PRG data of the `out.nes` file.

6. The NES ROM is now created. The reset vector will set up the stack pointers
   and restore the contents of RAM, continuing from where `freeze` left off. It will
   jump to the execution token stored in `thaw`.
   
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
