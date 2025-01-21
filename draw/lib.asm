section   .text

write_oooo:
    mov ah, 0x09
    mov al, 'O'   ;' write char directly
    mov bh, 1    ; page?
    mov bl, 0x06    ; color
    mov cx, 10      ; # times to write
    mov cx, 1      ; # times to write
    int 0x10        ; Call BIOS video interrupt
    ret