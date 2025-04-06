#!/bin/sh
gcc test_z80.c z80.c -o z80test.bin
if [ $? -eq 0 ]; then
    ./z80test.bin
fi
