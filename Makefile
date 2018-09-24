
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

out.nes: bootstrap.bin bootstrap.f font.chr subroutines.f game.f
	luajit 6502.lua bootstrap.f -b font:font.chr subroutines.f game.f

run: out.nes
	mono $(MESEN) out.nes
