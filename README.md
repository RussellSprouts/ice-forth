# Ice Forth

A Forth implementation self-hosted on the 6502, for creating NES roms.

Compiling a cartridge has several stages.

1. The assembly code (.s files) are compiled into an initial cartridge
   `bootstrap.bin`. This is a 64kb file containing the full memory space at
   startup for the next step. In that space we have the the basics of a Forth
   interpreter with an integrated assembler. (This only takes up a few kb of the
   full 64).

2. `6502.cpp` loads `bootstrap.bin` into its memory, and emulates running the
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

6. The NES ROM is now created. The word `thaw` is the startup word. It will restore
   the state of the machine and return as if returning from the word freeze.
   
## Features

- Not really compliant with ANS Forth :/
- Subroutine-threaded compilation
- Includes assembler and disassembler for NMOS 6502, written in 6502 assembly.
- Alpha state -- very buggy

## Running

Edit `Makefile` with the path to your [ca65 and ld65](https://cc65.github.io) executables.

```
$ make run
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
\ Disassembler the dup word
show-disas dup
```

```
Welcome to Forth!
\ Defining new words
: six  1 2 3 + + ;
```

```
Welcome to Forth!
\ Assembler usage
: oam-dma [
  hex
  sprites >byte LDA.#
  4014          STA
  decimal
] ;
```

Once you are ready to try the NES cartridge, use the word `done`.
`out/game.nes` will contain the ROM file. See game.f for a sample
of using the NES registers.

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
functions. Try `show-disas val` to the see the disassembly
for the `val` word.

### Assembler/Disassembler

The bootstrap assembly includes a 6502 assembler and disassembler.
Having all 130+ instructions in the dictionary would be too much,
so information about each instruction is stored in a more compact form.
The main data table stores only 4 nibbles (2 bytes) for each instruction.
These 4 values give the indices of the 3 opcode letters, and the addressing
mode. For example, from `disassembler.s`:

```
FirstLetter:  .byte "ABCDEIJLNOPRST"
```

Each instruction has a value from 0-13 indicating the index of the first
letter of the opcode in this list of letters. There are similar lists for
the second and third letters of each opcode. A slight complication arises
for the second letters. Among all of the opcodes, there are 18 different
letters used. The easiest way to work around this was to have special cases
for `txa`, `tya`, and `txs`, which are the only instructions which use
x or y as the second letter, and have only an implied addressing mode.
 This brings the list of second letters down to 16.

To save a bit more space, we make use of the fact that there are no
legal 6502 instructions which end in the binary sequence `%11`. These are
excluded from the table of instructions, giving 25% savings.

The syntax of the assembler is based on the [Typist's Assembler Notation](https://docs.google.com/document/d/16Sv3Y-3rHPXyxT1J3zLBVq4reSPYtY2G6OSojNTm4SQ/edit#). To indicate the addressing mode
of an instruction, a tail like `.ZX` is added, here indicating the ZeroPage,X
addressing mode.

When a dictionary lookup fails, the Forth interpreter will try to parse
the word as an instruction. If it succeeds, then it will either execute
or compile the instruction word. An instruction word has these semantics:

1. At runtime, appends the instruction and its arguments to the code area
at `chere`. For example, a Forth definition for LDA would look like this:

```
( addr -- )
: LDA  [ hex ] AD c, , ;
```

2. At compile time, compiles a call to `[asm]` (`RUN_ASM`), with inline
parameters indicating the opcode and number of argument bytes for the
instruction. `[asm]` will handle the runtime semantics of the instruction.

### The 6502 emulator

A simple emulator written in C++ is included. It is a very basic emulator,
probably has bugs even in basic operations, and not all instructions are
supported. (e.g. BRK and interrupts are unsupported). Like the NES, it doesn't
include the decimal mode.

To assist in debugging, these extra instructions are added:
- `$FF` enters trace mode. In trace mode, each instruction is logged with info about the machine state.
- `$EF` exits trace mode.
- `$DF` Takes a zero-terminated string as an argument, and prints it to the console.

An IO port at `$401C` powers the REPL. The emulator reads from stdin line-by-line,
and reads on `$401C` return the next byte from the input, blocking if there are none.
Writes are immediately sent to stdout.

The emulator exits cleanly when executing `jmp 0`. On exit, the emulator writes out several files:

- `out.nes` - The resulting NES file. This is the contents of memory at $8000-$FFFF on exit.
  The 16 byte iNES header is added.
- `ram.out` - The contents of RAM from $00-$7FF on exit, for debugging purposes
- `ops.out` - A log of the memory writes and IO input that the emulator has processed since startup.
  Using `visualize-ops.html`, you can see an animation of the Forth compiler working. It shows
  a 256x256 grid representing each byte as a single square, as well as each character that is read
  from the IO port.
