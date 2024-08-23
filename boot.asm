extern add_c

section .data
row5 dd 0xb8280 ; 5th row 
row6 dd 0xb8320 ; 6th row 

global start

section .text
bits 32    ; By default, GRUB loads the kernel in 32-bit mode
start:
    
    ; Print `Hello world!` on the screen by placing ASCII 
    ; characters in the VGA screen buffer that starts at 0xb8000

    ; 80×24 terminal
    mov word [0xb8000], 0x0248 ; H
    mov word [0xb8002], 0x1365 ; e
    mov word [0xb8004], 0x246c ; l
    mov word [0xb8006], 0x356c ; l
    mov word [0xb8008], 0x466f ; o
    mov word [0xb800a], 0x5720 ;
    mov word [0xb800c], 0xf077 ; w
    mov word [0xb800e], 0x026f ; o
    mov word [0xb8010], 0x0272 ; r
    mov word [0xb8012], 0x026c ; 
    mov word [0xb8014], 0x0264 ; d
    mov word [0xb8016], 0x0221 ; !
    mov word [0xb8018], 0x020a ; LF
    mov word [0xb8020], 0x0221 ; !
    mov word [0xb8022], 0x0265 ; e 

    mov word [0xb80a0], 0x0231 ; 1 ; second row - offset = 160 bytes = 0x00a0 

    mov word [0xb8140], 0x0232 ; 2 ; third row - offset = 320 bytes = 0x0140

    mov word [0xb81e0], 0x0233 ; 3 ; forth row - offset = 480 bytes = 0x01e0
    
    ;mov word [0xb8280], 0x0235 ; 5 ; fifth row - offset = 640 bytes = 0x0280

    
    mov eax, 1
    mov ebx, 4
    call _add_numbers ; sum cannot exceed 9...
    add eax, 0x0230

    ; Can't move directly from memory to memory
    ;mov dword [row5], eax 

    ; load the address 0xb8280 stored in 'result' into ebx insetad, then write output to address in ebx
    mov ebx, [row5]   
    mov dword [ebx], eax




    ; EXTERNAL C FUNCTION 'add_c(int x, int y)'
    
    ; This did not work
    ; mov eax, 1
    ; mov edx, 6

    ; Inspecting the c-object: '$ objdump -d build/add_c.o'
    ; The add_c routine is using the stack!
    mov eax, 3 ; parameter 1
    push eax   
    mov eax, 6 ; parameter 2
    push eax   
    call add_c 
    add esp, 8 ; reset stack pointer
    
    add eax, 0x0230 ; add to function return to render correct char 
    
    mov ebx, [row6]   
    mov dword [ebx], eax



    ; Did not work...
    ; call wait_for_keypress


    hlt ; Halt CPU 



_add_numbers:
    add eax, ebx
    ret



; GPT
wait_for_keypress:
    in al, 0x64           ; Read status from port 0x64
    test al, 0x01          ; Test if bit 0 (output buffer full) is set
    ; ME: jnz = jump if not zero
    jnz wait_for_keypress   ; If not, loop until a keypress occurs

    ; Read the scan code from the keyboard data port
    in al, 0x60            ; Read the scan code from port 0x60
    ; movsx [0xb8280], al
    
    ; Now AL contains the scan code of the pressed/released key
    
    