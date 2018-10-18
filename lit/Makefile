
# Change these configs
CC65=../../cc65/bin
MESEN=../../emus/mesen/Mesen.exe
LIT=../../literate/lit

out/bootstrap.bin: bootstrap.s
	$(CC65)/ca65 --cpu 6502x -g bootstrap.s -o out/bootstrap.o
	$(CC65)/ld65 --dbgfile out/debug.txt -C bootstrap.conf out/bootstrap.o -o out/bootstrap.bin

out/bootstrap.bin.h: out/bootstrap.bin
	xxd -i out/bootstrap.bin out/bootstrap.bin.h

out/6502: 6502.cpp
	g++ -O2 -Wall -Wpedantic -Wsequence-point -std=c++11 6502.cpp -o out/6502

out.nes: out/bootstrap.bin bootstrap.f font.chr game.f done.f out/6502
	./out/6502 bootstrap.f -b font:font.chr game.f done.f

run: out/game.nes
	mono $(MESEN) out/game.nes

repl: out/bootstrap.bin bootstrap.f font.chr game.f out/6502
	./out/6502 bootstrap.f -b font:font.chr game.f

lit: lit/test.lit
	$(LIT) lit/test.lit
