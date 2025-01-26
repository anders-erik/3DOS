#!/bin/bash

nasm -f bin -o mouse.bin mouse.asm

qemu-system-i386 -drive format=raw,file=mouse.bin
