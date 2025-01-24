section   .text

write_oooo:
    ; Set cursor position   
    ; AH=02h 	BH = Page Number, DH = Row, DL = Column
    ; mov al, 0
    ; mov ah, 0x02
    ; mov bh, 1
    ; mov dh, 10
    ; mov dl, 0
    ; int 0x10

    mov ah, 0x09
    mov al, 'O'   ;' write char directly
    mov bh, 1    ; page?
    mov bl, 0x06    ; color
    mov cx, 10      ; # times to write
    mov cx, 1      ; # times to write
    int 0x10        ; Call BIOS video interrupt
    ret


; single blue pixel
extern_pixel:
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x01  ; color -- Blue 
    mov cx, 50
    mov dx, 50
    int 0x10

    ; mov ax, 0xA000
    ; add ax, (320 * 50 + 50)
    ; mov [ax]
    
    mov ax, 0xA000         ; Segment for video memory
    mov es, ax             ; Point ES to video memory

    mov di, (320 * 51 + 51) ; Offset for pixel at (100, 50)
    mov al, 0x07           ; Pixel color (bright white)

    mov [es:di], al        ; Write pixel color to video memory

    ret
