section .data

; KEYBOARD INPUT CODE
key_code dw 0
key_code_ah dw 0
key_code_al dw 0
; mov WORD [key_code_ah], 0



; key-flags indicating that the key is currently pressed
press_event dw 0 ; 1 = pressed, 0 = released
w_pressed dw 0
mov WORD [w_pressed], 0x0000
a_pressed dw 0
s_pressed dw 0
d_pressed dw 0

ascii_current_press dw 0
cursor_buffer times 256 db 0
cursor_count dw 0
cursor_c dw 0
cursor_r dw 19
cursor_w dw 10
cursor_h dw 10


section .text


; Keyboard interrupt handler
; Will break the main loop and update the rendered content
keyboard_handler:

    pusha ; Need .286 direcctive? - 2024-10-31 - https://stackoverflow.com/questions/29728171/x86-assembly-set-of-pushes-and-pusha-difference
    in al, 0x60  ; Read keyboard scancode

    ; store key-information on initial interrupt
    mov [key_code], ax
    mov [key_code_ah], ah
    mov [key_code_al], al


    ; DETECT KEY PRESS OR RELEASE
    test al, 0x80      ; highest bit is press/release flag
    jnz .key_released  ; Bit was set = release

    .key_pressed:
    mov WORD [press_event], 0x1
    ; call .write_press_key_code_char
    call wasd_update

    ; Register ascii press
    mov ax, word [key_code_al]
    call store_ascii_pressed
    
    jmp .key_flag_done

    .key_released:
    mov WORD [press_event], 0x0
    sub BYTE [key_code], 0x80 ; get the key release value by subtracting release-flag
    sub BYTE [key_code_al], 0x80 ; get the key release value by subtracting release-flag
    call wasd_update
    ; call .write_release_key_code_char
    
    .key_flag_done:
    
    ; call float_tests


    mov al, 0x20
    out 0x20, al  ; Send EOI to PIC
    popa
    iret ; interrupt - meaning : 2024-10-31
_keyboard_handler:






; input: ax = raw keyboard press 'al' value 
store_ascii_pressed:
    ; mov ax, word [key_code_al]


._0: cmp ax, 0x0B
    jne ._1 
    mov word [ascii_current_press], '0'
    jmp .match

._1: cmp ax, 0x02
    jne ._2
    mov word [ascii_current_press], '1'
    jmp .match

._2: cmp ax, 0x03
    jne ._3
    mov word [ascii_current_press], '2'
    jmp .match

._3: cmp ax, 0x04
    jne ._4
    mov word [ascii_current_press], '3'
    jmp .match

._4: cmp ax, 0x05
    jne ._5
    mov word [ascii_current_press], '4'
    jmp .match

._5: cmp ax, 0x06
    jne ._6
    mov word [ascii_current_press], '5'
    jmp .match

._6: cmp ax, 0x07
    jne ._7 
    mov word [ascii_current_press], '6'
    jmp .match

._7: cmp ax, 0x08
    jne ._8
    mov word [ascii_current_press], '7'
    jmp .match

._8: cmp ax, 0x09
    jne ._9
    mov word [ascii_current_press], '8'
    jmp .match

._9: cmp ax, 0x0A
    jne .a
    mov word [ascii_current_press], '9'
    jmp .match

.a: cmp ax, 30
    jne .b
    mov word [ascii_current_press], 'A'
    jmp .match

.b: cmp ax, 0x30
    jne .c
    mov word [ascii_current_press], 'B'
    jmp .match

.c: cmp ax, 0x2E
    jne .d
    mov word [ascii_current_press], 'C'
    jmp .match

.d: cmp ax, 32
    jne .e
    mov word [ascii_current_press], 'D'
    jmp .match

.e: cmp ax, 18
    jne .f
    mov word [ascii_current_press], 'E'
    jmp .match

.f: cmp ax, 33
    jne .no_match ; NOTE THE CUSTOM END OF SWITCHING!
    mov word [ascii_current_press], 'F'
    jmp .match

.no_match:
    mov word [ascii_current_press], 0x00
    jmp .done


.match:
    ; write current press to cursor buffer
    mov ax, word [ascii_current_press]
    mov cx, word [cursor_count] ; current count
    mov bx, cursor_buffer
    add bx, cx
    mov byte [bx], al
    inc word [cursor_count]
.done:
    ret


; Toggle key states
wasd_update:


; SWITCH STATEMENT

.w:
    cmp WORD [key_code_al], 17 ; w = up
    jne .a

    cmp WORD [press_event], 1 ; is pressing
    je .w_press

.w_release:
    mov WORD [w_pressed], 0x0000
    jmp .wasd_done
    
.w_press:
    mov WORD [w_pressed], 1
    jmp .wasd_done



.a:
    cmp WORD [key_code_al], 30 ; a = left
    jne .s


    cmp WORD [press_event], 1 ; is pressing
    je .a_press

    .a_release:
    mov WORD [a_pressed], 0x0000
    jmp .wasd_done
    
    .a_press:
    mov WORD [a_pressed], 1
    jmp .wasd_done




.s:
    cmp WORD [key_code_al], 31 ; s = down
    jne .d

    cmp WORD [press_event], 1 ; is pressing
    je .s_press

    .s_release:
    mov WORD [s_pressed], 0x0000
    jmp .wasd_done
    
    .s_press:
    mov WORD [s_pressed], 1
    jmp .wasd_done



.d:
    cmp WORD [key_code_al], 32 ; d = right
    jne .next


    cmp WORD [press_event], 1 ; is pressing
    je .d_press

    .d_release:
    mov WORD [d_pressed], 0x0000
    jmp .wasd_done
    
    .d_press:
    mov WORD [d_pressed], 1
    jmp .wasd_done

    .next:

.wasd_done:

    ret



; Navingating using numpad
; checking key_code values against tested numpad input value, and change x/y accodingly
numpad_navigate:
    ; xor bx, bx
    ; xor cx, cx
    ; xor dx, dx
    ; make additional draw call based on keyboard input
    ; mov cx, 10 ; = pixel x location

    ; mov dx, 201
    cmp WORD [key_code], 72 ; numpad 8 = up
    je .up
    cmp WORD [key_code], 75 ; numpad 4 = left
    je .left
    cmp WORD [key_code], 76 ; numpad 5 = down
    je .down
    cmp WORD [key_code], 77 ; numpad 6 = right
    je .right

    jmp .not_equal ; no registered key

    .up:
        mov al, 0x02  ;  green
        sub DWORD [y], 2
        jmp .dn
    .left:
        mov al, 0x0D  ;  pink
        sub DWORD [x], 2
        jmp .dn
    .down:
        mov al, 0x01  ;  blue
        add DWORD [y], 2
        jmp .dn
    .right:
        mov al, 0x0E  ;  yellow
        add DWORD [x], 2
        jmp .dn

    .not_equal:
    mov al, 0x04  ;  color

    .dn:
    ; mov ah, 0x0C  ; BIOS video function: write pixel
    ; mov al, 0x0F  ; White color
    ; int 0x10

    ; Trying to guess values
    ; mov cx, [key_code]
    mov ah, 0x0C  ; BIOS video function: write pixel
    ; mov al, 0x0A  ; White color
    ; xor cx, cx
    ; xor dx, dx
    mov WORD cx, [x]
    mov WORD dx, [y]
    ; mov dx, y
    int 0x10
    ret


; Write some chars using AH=09h 
.write_release_key_code_char:
    ; Set cursor position
    ; AH=02h 	BH = Page Number, DH = Row, DL = Column
    mov ah, 0x02
    mov bh, 1
    mov dh, 6
    mov dl, 0
    int 0x10

    mov ah, 0x09
    mov al, [key_code]
    mov bh, 1    ; page?
    mov bl, 0x06    ; color
    mov cx, 2      ; # times to write
    int 0x10        ; Call BIOS video interrupt
    ret


; Write some chars using AH=09h 
.write_press_key_code_char:
    ; AH=02h 	BH = Page Number, DH = Row, DL = Column
    mov ah, 0x02
    mov bh, 1
    mov dh, 10
    mov dl, 0
    int 0x10

    mov ah, 0x09
    ; mov al, 'A'   ' write char directly
    mov al, [key_code]
    mov bh, 1    ; page?
    mov bl, 0x06    ; color
    ; mov cx, 10      ; # times to write
    mov cx, 1      ; # times to write
    int 0x10        ; Call BIOS video interrupt
    ret



