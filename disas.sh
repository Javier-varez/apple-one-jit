#!/bin/bash -e

rm -f .objfile
rust-objcopy -I binary -O elf64-aarch64 .binfile .objfile
rust-objdump -CD .objfile
