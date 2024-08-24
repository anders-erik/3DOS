kernel := build/kernel.bin

linker_script := linker.ld
assembly_source_files := $(wildcard *.asm)
assembly_object_files := $(patsubst %.asm, build/%.asm.o, $(assembly_source_files))

c_source_files := $(wildcard *.c)
c_object_files := $(patsubst %.c, build/%.c.o, $(c_source_files))


.PHONY: all clean kernel qemu qemu-gdb

all: $(kernel)

clean:
	- @rm -fr build *.o $(kernel)
	- @rm -f serial.log

qemu: $(kernel)
	qemu-system-x86_64 -vga std -s -serial file:serial.log -kernel $(kernel)

qemu-gdb: $(kernel)
	qemu-system-x86_64 -vga std -s -serial file:serial.log -S -kernel $(kernel)

$(kernel): $(assembly_object_files) $(c_object_files) $(linker_script)
	ld -m elf_i386 -n -T $(linker_script) -o $(kernel) $(assembly_object_files) $(c_object_files)

# compile assembly files
build/%.asm.o: %.asm
	@mkdir -p $(shell dirname $@)
	nasm -felf32 $< -o $@

# compile c files
build/%.c.o: %.c
	@mkdir -p $(shell dirname $@)
	gcc -c -m32 $< -o $@