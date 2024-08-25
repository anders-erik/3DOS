# For attaching debugger
target remote localhost:1234
symbol-file ./build/kernel.bin

# binary print debug
set disassembly-flavor intel
lay next
layout reg
# break print_word_to_vga
break print_loop
# break index_is_zero
c

