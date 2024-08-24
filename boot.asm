
extern add_c

section .data
row5 dd 0xb8280 ; 5th row 
row6 dd 0xb8320 ; 6th row 

row db 0          ; The row where you want to print (0-indexed)
col db 0          ; The column where you want to print (0-indexed)
color db 0x35     ; Color attribute byte (light grey on black)
number dd 0x1234  ; number to print

global print_word_to_vga
global start
; global start2

section .text
bits 32    ; By default, GRUB loads the kernel in 32-bit mode



start:
    
    ; Print `Hello world!` on the screen by placing ASCII 
    ; characters in the VGA screen buffer that starts at 0xb8000

    ; 80×24 terminal
    ; mov word [0xb8000], 0x0248 ; H
    ; mov word [0xb8002], 0x1365 ; e
    ; mov word [0xb8004], 0x246c ; l
    ; mov word [0xb8006], 0x356c ; l
    ; mov word [0xb8008], 0x466f ; o
    ; mov word [0xb800a], 0x5720 ;
    ; mov word [0xb800c], 0xf077 ; w
    ; mov word [0xb800e], 0x026f ; o
    ; mov word [0xb8010], 0x0272 ; r
    ; mov word [0xb8012], 0x026c ; 
    ; mov word [0xb8014], 0x0264 ; d
    ; mov word [0xb8016], 0x0221 ; !
    ; mov word [0xb8018], 0x020a ; LF
    ; mov word [0xb8020], 0x0221 ; !
    ; mov word [0xb8022], 0x0265 ; e 

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
    mov eax, 2 ; parameter 1
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

    ;   
    ; sti  ; Enable interrupts

    ; mov ah, 0x00   ; Function: Set Video Mode
    ; mov al, 0x02   ; Mode 13h (320x200, 256 colors)
    ; int 0x10       ; Call BIOS interrupt

    mov word [0xA0000], 0x03 ; e 

    ; mov ah, 0x00   ; Function: Set Video Mode
    ; mov al, 0x03   ; Mode 03h (80x25, text mode)
    ; int 0x10       ; Call BIOS interrupt
    
    ; cli  ; Disable interrupts


    ; PRINT
    mov ax, [number]
    jmp print_word_to_vga
wrap_up:
    xor eax, eax
    mov word [0xb8000], 0x0233
    mov eax, [0xb8000]
    mov [0xb8002], eax
    ; hlt ; Halt CPU 
    ; .die
    ; hlt
    ; jmp .die
    haltloop: 
    hlt 
    jmp haltloop


_add_numbers:
    add eax, ebx
    ret



; PRINT SOUBROUTINE
print_word_to_vga:
    pusha                     ; Save all general-purpose registers
    xor eax, eax
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx
    xor edi, edi
    xor esi, esi
    ; mov ebx, 0xb8000           ; VGA memory base address
    xor ecx, ecx                ; Clear CX (used as a counter)
    mov cx, 16                ; We will print 16 characters (bits)

    mov si, word [row]        ; Load row value into SI
    mov di, word [col]        ; Load col value into DI

    ; Calculate the offset in VGA memory
    ; movzx : move into wider word, fill upper bits with zeros
    movzx si, byte [row]      ; Row * 80 (bytes per line)
    movzx di, byte [col]      ; Column offset 
    mov ax, si
    mov bx, 80
    mul bx  ; ax = ax * bx
    add ax, di
    ; Shift left 1 = squre number
    shl ax, 1                 ; Each character takes 2 bytes (character + attribute)
    ; add bx, ax                ; Add to VGA base address
    ; ME next 4
    add eax, 0xb8000 ; ME: base address plus offset
    mov ebx, eax      ; ME: store the print location in bx permanently
    xor eax, eax ; zero
    mov ax, [number]; ME : number to be printed in ax - the dermines if '1' or '0'

print_loop:
    dec cx
    jz index_is_zero
    rol ax, 1                 ; Rotate the most significant bit to the carry flag
    jc  bit_is_set            ; If carry flag is set, bit is 1
    ; mov al, '0'               ; Otherwise, print '0'
    mov dl, '8'               ; Otherwise, print '0'
    jmp print_char

bit_is_set:
    ; mov al, '1'               ; Print '1'
    mov dl, '7'               ; dx holds character value

print_char:
    ; mov ah, [color]           ; Load color attribute
    mov dh, [color]           ; Load color attribute
    ; mov [bx], ax              ; Write the character and attribute to VGA memory
    mov [bx], dx              ; Write the character and attribute to VGA memory
    ; xor r8, r8        ; debug
    ; mov r8, [0xb8000]  ; debug
    add bx, 2                 ; Move to the next character position
    loop print_loop           ; Repeat for all 16 bits

index_is_zero:
    popa                      ; Restore all general-purpose registers
    jmp wrap_up





start2:
    mov word [0xb8000], 0x0248 ; H
    hlt 

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
    
    