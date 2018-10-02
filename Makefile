
# Change these configs
CC65=../cc65/bin
MESEN=../emus/mesen/Mesen.exe

S_FILES=note.s bootstrap.s neslib.s coroutines.s disassembler.s end.s

out/bootstrap.bin: $(S_FILES)
	$(CC65)/ca65 --cpu 6502x -g note.s -o out/note.o
	$(CC65)/ca65 --cpu 6502x -g bootstrap.s -o out/bootstrap.o
	$(CC65)/ca65 --cpu 6502x -g neslib.s -o out/neslib.o
	$(CC65)/ca65 --cpu 6502x -g coroutines.s -o out/coroutines.o
	$(CC65)/ca65 --cpu 6502x -g disassembler.s -o out/disassembler.o
	$(CC65)/ca65 --cpu 6502x -g end.s -o out/end.o
	$(CC65)/ld65 --dbgfile out/debug.txt -C config out/note.o out/bootstrap.o out/neslib.o out/coroutines.o out/disassembler.o out/end.o -o out/bootstrap.bin

out/bootstrap.bin.h: out/bootstrap.bin
	xxd -i out/bootstrap.bin out/bootstrap.bin.h

out/6502: 6502.cpp
	g++ -O2 -Wall -Wpedantic -Wsequence-point -std=c++11 6502.cpp -o out/6502

out.nes: out/bootstrap.bin bootstrap.f font.chr subroutines.f game.f done.f out/6502
	./out/6502 bootstrap.f -b font:font.chr subroutines.f game.f done.f

run: out/game.nes
	mono $(MESEN) out/game.nes

repl: out/bootstrap.bin bootstrap.f font.chr subroutines.f game.f out/6502
	./out/6502 bootstrap.f -b font:font.chr subroutines.f game.f
