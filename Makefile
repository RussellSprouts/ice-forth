
# Change these configs
CC65=../cc65/bin
MESEN=../emus/mesen/Mesen.exe

S_FILES=note.s bootstrap.s neslib.s coroutines.s disassembler.s end.s

bootstrap.bin: $(S_FILES)
	$(CC65)/ca65 --cpu 6502x -g note.s
	$(CC65)/ca65 --cpu 6502x -g bootstrap.s
	$(CC65)/ca65 --cpu 6502x -g neslib.s
	$(CC65)/ca65 --cpu 6502x -g coroutines.s
	$(CC65)/ca65 --cpu 6502x -g disassembler.s
	$(CC65)/ca65 --cpu 6502x -g end.s
	$(CC65)/ld65 --dbgfile debug.txt -C config note.o bootstrap.o neslib.o coroutines.o disassembler.o end.o -o bootstrap.bin

6502: 6502.cpp
	g++ -O2 -Wall -Wpedantic -Wsequence-point -std=c++11 6502.cpp -o 6502

out.nes: bootstrap.bin bootstrap.f font.chr subroutines.f game.f done.f 6502
	./6502 bootstrap.f -b font:font.chr subroutines.f game.f done.f

run: out.nes
	mono $(MESEN) out.nes

repl: bootstrap.bin bootstrap.f font.chr subroutines.f game.f 6502
	./6502 bootstrap.f -b font:font.chr subroutines.f game.f
