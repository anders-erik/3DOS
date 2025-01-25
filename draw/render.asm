section   .text

render:
    push bp
    mov bp, sp

    ; pusha
    call clear
    ; call clear_screen_old

    call draw_wasd_input

    ; call draw_tests
    ; call draw_large_square
    call mode_13h_pixel_draw
    call draw_input_incrementing_pixel
    call draw_keycode_coords ; uses interrupts 
    call extern_pixels
    call simple_pixel

    ; draw player position
    .draw_player_position:
    push 0x01   ; color
    push word [player_position_y]    ; y
    push word [player_position_x]    ; x
    call draw_2x2
    add sp, 6

    push 0x00   ; color
    push  10    ; y
    push  10    ; x
    call draw_2x2
    add sp, 6



    push 0x00   ; color
    push 100    ; y
    push 100    ; x
    call draw_2x2
    add sp, 6
    ; Functional Stack reset alternatives:
    ; #1
    ; pop ax
    ; pop ax
    ; pop ax
    ; #2
    ; add sp, 4
    ; pop ax
    ; #3 
    ; add sp, 6

    ; Tried to print. No success...
    ; mov di, es
    ; mov ax, 0xb800
    ; mov es, ax
    ; mov word [es:0x0000], 0x0248 ; H
    ; mov word [es:0x0000], 0x1365 ; e
    ; mov es, di

    

    mov sp, bp  ; return stack pointer
    pop bp      ; restore bp to callers value
    ; popa
    ret



; @ x [i16] - bp + 4
; @ y [i16] - bp + 6
; @ c [i16] - bp + 8
draw_2x2:

    ; save previous stack frame. Then set up new one. 
    push bp
    mov bp, sp ; can't use stack pointer for effective address calculation
    
    mov ax, 0xA000         ; Segment for video memory
    mov es, ax             ; Point ES to video memory

    ; Set color
    mov dl, BYTE [bp + 8] ; col 1
    mov dh, BYTE [bp + 8] ; col 2

    ; set first row y value
    mov di, 320
    mov ax, WORD [bp + 6]
    imul di, ax ; y

    mov ax, WORD [bp + 4]
    add di, ax ; x

    mov [es:di], dx     ; write first pixel row

    ; second row 
    add di, 320
    mov [es:di], dx     ; write second pixel row

    
    ; reset stack to call entrypoint
    mov sp, bp
    pop bp

    ; Option # 1
    pop ax ; return adress
    jmp ax
    ; Option # 2
    ret


draw_wasd_input:

.w:
    cmp WORD [w_pressed], 1
    jne .a

    ; .w_draw_press:
    ; mov ax, 20
    ; mov bx, 162
    ; mov si, 0x02
    ; call pixel_x_cx_y_dx_c_si
    ; jmp .a

    mov ax, 20
    mov bx, 160
    mov cx, 5 ; width
    mov dx, 5 ; height
    mov si, 0x02
    call draw_square



    .a:
    cmp WORD [a_pressed], 1 ; a = left
    jne .s

    ; mov ax, 17
    ; mov bx, 165
    ; mov si, 0x04
    ; call pixel_x_cx_y_dx_c_si
    ; jmp .wasd_done
    mov ax, 15
    mov bx, 165
    mov cx, 5 ; width
    mov dx, 5 ; height
    mov si, 0x04
    call draw_square


    .s:
    cmp WORD [s_pressed], 1 ; s = down
    jne .d

    ; mov ax, 20
    ; mov bx, 165
    ; mov si, 0x0F
    ; call pixel_x_cx_y_dx_c_si
    ; jmp .wasd_done

    mov ax, 20
    mov bx, 165
    mov cx, 5 ; width
    mov dx, 5 ; height
    mov si, 0x0F
    call draw_square



    .d:
    cmp WORD [d_pressed], 1 ; d = right
    jne .next

    ; mov ax, 23
    ; mov bx, 165
    ; mov si, 0x03
    ; call pixel_x_cx_y_dx_c_si

    ; SQUARE TEST
    mov ax, 25
    mov bx, 165
    mov cx, 5 ; width
    mov dx, 5 ; height
    mov si, 0x03
    call draw_square

    .next:

    


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





; draw square 'local' variables
; I don't have a stack yet...
sq_hh dw 0
sq_ww dw 0
sq_xx dw 0
sq_yy dw 0
sq_cc dw 0
; Draw a square at (ax, bx), with (h, w)=(cx, dx), and color=si
draw_square:
    ; 'push' arguments
    mov [sq_xx], ax
    mov [sq_yy], bx
    mov [sq_ww], cx
    mov [sq_hh], dx
    mov [sq_cc], si

; video segment
    mov ax, 0xA000
    mov es, ax


; row-index
    mov cx, 0
.L_ROW:
    ; Set di to point to leftmost in current row
    mov di, [sq_yy]
    add di, cx
    imul di, 320 ; y
    add di, [sq_xx] ; x
    

; column-index
    mov dx, 0
.L_COLUMN:

    ; DRAW PIXEL
    mov al, [sq_cc] ; color
    mov [es:di], al ; location

    ; next pixel location -- x-direction
    ; increment AFTER draw to properly draw with zero index
    inc di

    ; increment col-index until height is reached
    inc dx
    cmp dx, WORD [sq_ww]
    jl .L_COLUMN

.L_COLUMN_END:  

    ; keep incrementing row-index until height is reached
    inc cx
    cmp cx, WORD [sq_hh]
    jl .L_ROW 

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
