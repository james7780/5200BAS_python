@ECHO OFF
tasm -65 -b -fFF -q %1.asm %1.bin
DEL %1.LST
