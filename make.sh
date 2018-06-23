#!/usr/bin/env bash

../game/cc65/bin/ca65 --cpu 6502x -g bootstrap.s
../game/cc65/bin/ld65 --dbgfile debug.txt -C config bootstrap.o -o fig

