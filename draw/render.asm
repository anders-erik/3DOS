section   .text

render:
    push bp
    mov bp, sp

    ; pusha
    call clear
    ; call clear_screen_old

    call draw_triangle
    ; cli 

    ; hlt

    call draw_wasd_input

    ; call draw_tests
    ; call draw_large_square
    call mode_13h_pixel_draw
    call draw_input_incrementing_pixel
    call draw_keycode_coords ; uses interrupts 
    call extern_pixels
    call simple_pixel

    ; draw player position
    call draw_player_position
    

    call a2x2_cluster


    ; print current cursor buffer
    call write_whole_cursor_buffer
    

    ; Print once when new ascii press detected
    call show_current_ascii_press


    call print_available_chars


    mov sp, bp  ; return stack pointer
    pop bp      ; restore bp to callers value
    ; popa
    ret



show_current_ascii_press:
    cmp word [ascii_current_press], 0
    je .write_current_press_end
    mov ax, word [ascii_current_press]
    call write_ascii_char_at_cursor
    mov word [ascii_current_press], 0 ; reset current press value as it is acting as press-flag
    .write_current_press_end:
    ret


a2x2_cluster:
    push 0x00   ; color
    push  105    ; y
    push  105    ; x
    call draw_2x2
    add sp, 6

    push 0x00   ; color
    push  103    ; y
    push  101    ; x
    call draw_2x2
    add sp, 6

    push 0x00   ; color
    push  101    ; y
    push  104    ; x
    call draw_2x2
    add sp, 6

    push 0x00   ; color
    push 100    ; y
    push 100    ; x
    call draw_2x2
    add sp, 6

    ret

draw_player_position:
    push 0x01   ; color
    push word [player_position_y]    ; y
    push word [player_position_x]    ; x
    call draw_2x2
    add sp, 6
    ret


print_available_chars:

    mov ax, 'A'
    call char_ascii_to_bitmap_address
    ; mov si, ax          ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 10             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'B'
    call char_ascii_to_bitmap_address
    ; mov si, char_B          ; Letter address
    push ax          ; Letter address
    push word 190           ; y
    push word 20            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'C'
    call char_ascii_to_bitmap_address
    mov si, char_C      ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 30             ; x
    call write_char_from_bitmap_address
    add sp, 6

    ret

; loop throught the cursor buffer, printing output at fixed row (2?)
write_whole_cursor_buffer:

    mov bx, cursor_buffer       ; buffer address
    ; Indexes
    mov cx, word [cursor_count] ; loop decrement - index
    mov si, 0x0000              ; current char index in buffer
    .print_cursor_buffer:
    

    ; move char from buffer and get char address
    mov al, byte [bx + si]
    xor ah, ah
    call char_ascii_to_bitmap_address

    ; set cursor x location
    mov dx, si                  ; buffer index
    imul dx, word [cursor_w]    ; cursor width

    ; set cursor y location (fixed for now)
    mov di, 18 ; second row
    imul di, word [cursor_h]
    

    ; perists across call
    push si
    push cx

    push ax             ; char bitmap address
    push di             ; y
    push dx             ; x
    call write_char_from_bitmap_address
    add sp, 6

    pop cx
    pop si

    ; call 
    ; mov ax, word [ascii_current_press]
    ; call write_char_at_cursor

    inc si
    dec cx
    cmp word cx, 0
    jg .print_cursor_buffer
    ; loop .print_cursor_buffer

    ; DEBUG
    ; cli 
    ; hlt

    ret


; input     : ax = char ascii value
; WILL ONLY RENDER FOR NEXT FRAME ONE FRAME!
; NO INCREMENT
write_ascii_char_at_cursor:
    ; mov ax, 'A'
    call char_ascii_to_bitmap_address
    ; ax = bitmap address
    
    ; set cursor x location
    mov bx, word [cursor_c]
    ; inc word [cursor_c]
    imul bx, word [cursor_w]

    ; set cursor y location (fixed for now)
    mov dx, word [cursor_r]
    imul dx, word [cursor_h]

    ; mov si, ax          ; Letter address
    push ax
    push dx            ; y
    push bx             ; x
    call write_char_from_bitmap_address
    add sp, 6

    ret


; input     : ax = char ascii value
; return    : ax = bitmap address
char_ascii_to_bitmap_address:
.A: cmp ax, 0x41
    jne .B
    mov ax, char_A
    jmp .done

.B: cmp ax, 0x42
    jne .C
    mov ax, char_B
    jmp .done

.C: cmp ax, 0x43
    jne .default ; NOTE THE CUSTOM END OF SWITCHING!
    mov ax, char_C
    jmp .done

.D:

.default:
    mov ax, char_default
    jmp .done

.done:
    ret


; routine is heavily commented for the sake of learning!
write_char_from_bitmap_address:
    push bp
    mov bp, sp

    ; char bitmap address
    mov si, [bp+8]

    ; set up buffer segment
    mov ax, 0xA000
    mov es, ax


    mov cx, 8               ; row loop inex == char height
.draw_row:
    lodsb                   ; load letter byte (si) into ax
    push word cx            ; Save row counter, as cx will be used for column indexing loop

    ; Set leftmost location of new row
    mov di, [bp+6]          ; Start at the Y-coordinate
    imul di, 320            ; Multiply Y by screen width (320)
    add di, [bp+4]          ; Add X-coordinate to get pixel offset

    mov cx, 8               ; Column loop index == char width
.draw_pixel:
    test al, 10000000b      ; Test the most significant bit (1 pixel)
    jz .next_pixel          ; If 0, skip drawing the pixel

    mov byte [es:di], 0x0F  ; DRAW PIXEL

.next_pixel:                ; always move to next pixel
    shl al, 1               ; next letter bit for comparison
    inc di                  ; Move to the next framebuffer byte
    loop .draw_pixel        ; Repeat for all 8 pixels in the row

    inc word [bp+6]         ; Move to the next row by incrementing y

    pop word cx             ; Restore row counter
    loop .draw_row          ; dec cx + cmp cx, 0 + jnz .draw_row ?


    mov sp, bp
    pop bp
    ret
write_a_end:


draw_triangle:
    ; pusha

    ; Bottom left corner
    push word [tri_2d_int_array+4]   ; color
    push word [tri_2d_int_array+2]    ; y
    push word [tri_2d_int_array+0]    ; x
    call draw_2x2
    add sp, 6

    ; bottom right corner
    push word [tri_2d_int_array+10]   ; color
    push word [tri_2d_int_array+8]    ; y
    push word [tri_2d_int_array+6]    ; x
    call draw_2x2
    add sp, 6

    ; Top left corner
    push word [tri_2d_int_array+16]   ; color
    push word [tri_2d_int_array+14]    ; y
    push word [tri_2d_int_array+12]    ; x
    call draw_2x2
    add sp, 6


    ; draw every pixel below the line connecting top left and bottom right


    ;
    ; get slope from point 3 to point 1
    ;

    ; delta x
    mov ax, [tri_2d_int_array+6]
    sub ax, [tri_2d_int_array+12]
    ; delta y
    mov bx, [tri_2d_int_array+8]
    sub bx, [tri_2d_int_array+14]

    ; slope using integers
    ; slope = si / 10
    mov si, 0x0000
    mov dx, bx
    imul dx, 10
    ; count number of dx in dy*10
    mov cx, 0
.slope_calc_loop:
    add cx, ax
    inc si
    cmp cx, dx
    jle .slope_calc_loop
.slope_calc_loop_end:

    ; start drawing pixels at si
    mov di, si




    mov ax, 0xA000
    mov es, ax
    ; xor ax, ax
    ; mov ax, 0x0000
    ; mov di, 0x0000
    ; imul di, 320
    ; mov di, dx
    mov dx, 0xF900
    ; mov dx, 320*200

    ; mov ax, [tri_2d_int_array+4] ; color of first triangle
    mov ax, 0x12
    .loop_start:
    inc di
    mov [es:di], al

    ; Split screen diagonally


    ; ONLY works if set to 'not equal'!
    ; spent an hour trying to use jle/jge but was only ableto draw top or bottom half of screen....
    cmp word di, dx
    ; jne .loop_start
    ; http://unixwiz.net/techtips/x86-jumps.html
    ; keep looping as long as di is less than dx
    ; UNSIGNED, and CARRY DETECTED as dx is greater??
    jb .loop_start
    
    .loop_end:

    ; popa
    ret
draw_triangle_end:



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
    
    ; Increments di by 2 each iteration (default DF=0) and loads ax into [es:di] and stops at di=cx?
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
