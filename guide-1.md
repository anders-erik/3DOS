# 1. Qemu+kvm - Anton Burtsev
https://mars-research.github.io/posts/2020/10/hello-world-on-bare-metal/

### Software used
- nasm
- qemu
- kvm
- gdb
- ld

## Setup

### Commands - 2024-08-22
```
$ sudo apt install nasm 
// 3 345 kB of additional disk space

$ sudo apt install nasm 
// already installed

$ sudo apt install qemu-system-x86
// 125 MB of additional disk space
```


## 1st attempt - 2024-08-22

### multiheader compile - OK

```
// copy 'multiboot_header.asm' from guide

$ nasm -felf32 multiboot_header.asm -o multiboot_header.o
```

### compile 'boot.asm' - OK
```
$ nasm -felf32 boot.asm -o boot.o

```

### Build first ELF binary - OK
```
// create 'linker.ld' file

$ ld -m elf_i386 -n -T linker.ld -o kernel.bin boot.o multiboot_header.o

```

### Run First hello world build = XX
```
qemu-system-x86_64 -kernel kernel.bin
// qemu-system-x86_64: symbol lookup error: /snap/core20/current/lib/x86_64-linux-gnu/libpthread.so.0: undefined symbol: __libc_pthread_init, version GLIBC_PRIVATE

```

https://github.com/ros2/ros2/issues/1406
> The problem was mainly because of the VSCode's environment variable ... I unset the GTK_PATH environment variable for VSCode using ... uset GTK_PATH

> This did not solved the problem in my case. My solution was to uninstall VSCode (which was previously installed from the Ubuntu Store - snap) and download the .deb directly and install it with apt.

```
$ echo $GTK_PATH
// /snap/code/167/usr/lib/x86_64-linux-gnu/gtk-3.0
```
Obviously snap/code is hogging on GTK_PATH, but how would I know it's that veriable thats causing probems??
The message clearly shows that 'code' is hijacking the veriable that qemu needs. So let's try to reinstall VSCode without snap...





