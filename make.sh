#!/usr/bin/env bash

../cc65/bin/ca65 --cpu 6502x -g note.s
../cc65/bin/ca65 --cpu 6502x -g bootstrap.s
# ../cc65/bin/ca65 --cpu 6502x -g neslib.s
../cc65/bin/ca65 --cpu 6502x -g disassembler.s
../cc65/bin/ld65 --dbgfile debug.txt -C config note.o bootstrap.o disassembler.o -o fig

