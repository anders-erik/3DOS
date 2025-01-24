section   .text

render:
    pusha
    call clear
    ; call clear_screen_old

    call draw_wasd_input

    ; call draw_tests
    ; call draw_large_square
    call mode_13h_pixel_draw
    call draw_input_incrementing_pixel
    call draw_keycode_coords
    call extern_pixels
    call simple_pixel
    popa
    ret


draw_wasd_input:

.w:
    cmp WORD [w_pressed], 1
    jne .a

    .w_draw_press:
    mov ax, 20
    mov bx, 162
    mov si, 0x01
    call pixel_x_cx_y_dx_c_si
    jmp .a



    .a:
    cmp WORD [a_pressed], 1 ; a = left
    jne .s

    mov ax, 17
    mov bx, 165
    mov si, 0x04
    call pixel_x_cx_y_dx_c_si
    ; jmp .wasd_done



    .s:
    cmp WORD [s_pressed], 1 ; s = down
    jne .d

    mov ax, 20
    mov bx, 165
    mov si, 0x0F
    call pixel_x_cx_y_dx_c_si
    ; jmp .wasd_done



    .d:
    cmp WORD [d_pressed], 1 ; d = right
    jne .next

    mov ax, 23
    mov bx, 165
    mov si, 0x03
    call pixel_x_cx_y_dx_c_si

    .next

    .wasd_done:

    ret

; Clear the screen
clear_screen_old:
    pusha
    mov ah, 0x06    ; Scroll up function
    mov al, 0       ; Clear entire screen
    mov bh, 0x08    ; dark gray
    mov ch, 0       ; Upper left row
    mov cl, 0       ; Upper left column
    mov dh, 24      ; Lower right row
    mov dl, 79      ; Lower right column
    int 0x10        ; Call BIOS video interrupt
    popa
    ret


draw_tests:
; 2024-10-31 : six lines added to add two new pixels!
    mov cx, 2     ; X position for second pixel
    mov dx, 2     ; Y position for second pixel
    int 0x10
    mov cx, 3     ; X position for second pixel
    mov dx, 3     ; Y position for second pixel
    int 0x10


    mov cx, 3     ; X position for second pixel
    mov dx, 3     ; Y position for second pixel
    int 0x10

    ; 2024-10-31 : testing array
    mov cx, [word_array]     ; array pos. 5
    mov dx, [word_array]     ; array pos. 5
    ; mov cx, 10     ; array pos. 5
    ; mov dx, 10     ; array pos. 5
    int 0x10



; Draw large square -- INTERRUPT BASED
draw_large_square:
    mov ah, 0x06    ; Scroll up 
    mov bh, 0x06    ; color
    mov ch, 10      ; Upper left x of square
    mov cl, 10      ; Upper left y of square
    mov dh, 20      ; Lower right x of square
    mov dl, 20      ; Lower right y of square
    int 0x10        ; Call BIOS video interrupt
    ret


; Draw a pixel at (x, y) with color
; mode h13 is set at the beginning of the program
mode_13h_pixel_draw:
    mov ax,0a000h
    mov es,ax
    mov ax,20    ; y = 20
    mov bx,20
    shl ax,8
    shl bx,6
    add ax,bx
    add ax,30    ; x = 30
    mov di,ax
    mov al,2    ; color = 2 = green
    mov es:[di],al
    ret

; Draw the 'current' pixel 
draw_input_incrementing_pixel:

    mov ax, [pixel_x]
    mov bx, [pixel_y]
    mov si, 0x0F
    call pixel_x_cx_y_dx_c_si

    ret


;  try to understand the key_code values
; Draw al as x, and ah as y
draw_keycode_coords:
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0A  ; White color
    xor cx, cx
    xor dx, dx
    mov cx, [key_code_al]
    mov dx, [key_code_ah]
    int 0x10
    ret

; draw pixel test
simple_pixel:
    mov ax, 100
    mov bx, 70
    mov si, 0x0f
    call pixel_x_cx_y_dx_c_si
    ret

clear:
    ; clear screen using graphics mode writing directly to video buffer
    mov ax, 0xA000         ; Segment for graphics video memory
    mov es, ax             ; Point ES to video memory
    mov di, 0              ; Start at the top-left corner
    mov cx, 320 * 200 / 2  ; rep stosw increments two bytes per iteration
    mov ax, 0xCFCF ; 2 x mode 13h color palette (1 byte / pixel)
    
    ; Increments di by 2 each iteration (default DF=0) and loads eax into [es:di] and stops at di=cx?
    ; stosw : w=word=eax, stosb : w=byte,
    rep stosw

    ret



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




; draw pixel variables
xx dw 0
yy dw 0
cc dw 0
; Draw a pixel at (cx, dx), with color (si)
pixel_x_cx_y_dx_c_si:
    mov [xx], ax
    mov [yy], bx
    mov [cc], si

    mov ax, 0xA000         ; Segment for video memory
    mov es, ax             ; Point ES to video memory


    mov di, 320
    imul di, [yy] ; y
    add di, [xx] ; x
    mov al, [cc]           ; Pixel color (bright white)

    mov [es:di], al        ; Write pixel color to video memory
    ret
 

; single blue and white pixels
extern_pixels:
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
