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

    mov word [0xb81e0], 0x0233 ; 3 ; third row - offset = 480 bytes = 0x01e0
    
    mov word [0xb8280], 0x0235 ; 4 ; forth row - offset = 640 bytes = 0x0280

    ; call wait_for_keypress



    hlt ; Halt CPU 
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
    
    