#!/usr/bin/env bash

../game/cc65/bin/ca65 --cpu 6502x -g note.s
../game/cc65/bin/ca65 --cpu 6502x -g bootstrap.s
../game/cc65/bin/ca65 --cpu 6502x -g disassembler.s
../game/cc65/bin/ld65 --dbgfile debug.txt -C config note.o bootstrap.o disassembler.o -o fig

