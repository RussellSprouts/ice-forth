#!/usr/bin/env bash

../game/cc65/bin/ca65 bootstrap.s
../game/cc65/bin/ld65 -C config bootstrap.o -o fig

