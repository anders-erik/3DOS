
// gcc -c -m32 -o build/add_c.o add_c.c 
// Also manually add to makefile linking command

// extern add_c ; add this to assembly file
// remember that this assumes that the function parameters are on the stack!
//      (objdump -d build/add_c.o)
// Intel syntax: objdump -d -M intel build/add_c.c.o

int add_c(int x, int y){
    return x + y;
}