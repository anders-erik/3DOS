; Double buffering segment
VIDEO_D_BUFFER equ 0x7000

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
    ; call mode_13h_pixel_draw
    call draw_input_incrementing_pixel
    ; call draw_keycode_coords ; uses interrupts 
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

    ; mov ax, 0x1234
    ; mov ax, 0xfa74
    ; call print_hex_value

    call draw_sine

    ; call draw_current_triangle


    ; Triangle 1
    call update_triangle_velocity
    call update_triangle_position

    call transform_triangle_1_into_current
    call current_triangle_into_pixel_coord
    ; call draw_current_triangle_points
    call draw_current_triangle



    call swap_buffer

    mov sp, bp  ; return stack pointer
    pop bp      ; restore bp to callers value
    ; popa
    ret


update_triangle_velocity:
    fld dword [position+0] ; p_x 
    fld dword [velocity+0] ; v_x 
    faddp
    fstp dword [position+0]
    ret
update_triangle_velocity_end:


update_triangle_position:
    fld dword [position+0] ; p_x 
    fld dword [velocity+0] ; v_x 
    faddp
    fstp dword [position+0]
    ret
update_triangle_position_end:

;-- set_current_triangle_loop_span  ---
;
;   Set the x/y bounding box mins and maxs for current triangle.
;   
set_current_triangle_loop_span:
    mov word [p_x_int_min], 320
    mov word [p_x_int_max], 0
    mov word [p_y_int_min], 200
    mov word [p_y_int_max], 0

.p0_x_min:
    mov ax, word [p_x_int_min]
    cmp word [p0_x_int], ax
    jae .p1_x_min ; mov if not below
    mov ax, word [p0_x_int]
    mov word [p_x_int_min], ax

.p1_x_min:
    mov ax, word [p_x_int_min]
    cmp word [p1_x_int], ax
    jae .p2_x_min ; mov if not below
    mov ax, word [p1_x_int]
    mov word [p_x_int_min], ax

.p2_x_min:
    mov ax, word [p_x_int_min]
    cmp word [p2_x_int], ax
    jae .x_min_done ; mov if not below
    mov ax, word [p2_x_int]
    mov word [p_x_int_min], ax
.x_min_done: 


.p0_x_max:
    mov ax, word [p_x_int_max]
    cmp word [p0_x_int], ax
    jbe .p1_x_max ; new max if above current
    mov ax, word [p0_x_int]
    mov word [p_x_int_max], ax

.p1_x_max:
    mov ax, word [p_x_int_max]
    cmp word [p1_x_int], ax
    jbe .p2_x_max ; new max if above current
    mov ax, word [p1_x_int]
    mov word [p_x_int_max], ax

.p2_x_max
    mov ax, word [p_x_int_max]
    cmp word [p2_x_int], ax
    jbe .x_max_done ; new max if above current
    mov ax, word [p2_x_int]
    mov word [p_x_int_max], ax
.x_max_done: 


.p0_y_min:
    mov ax, word [p_y_int_min]
    cmp word [p0_y_int], ax
    jae .p1_y_min ; new min if below current
    mov ax, word [p0_y_int]
    mov word [p_y_int_min], ax

.p1_y_min:
    mov ax, word [p_y_int_min]
    cmp word [p1_x_int], ax
    jae .p2_y_min ; new min if below current
    mov ax, word [p1_x_int]
    mov word [p_y_int_min], ax

.p2_y_min
    mov ax, word [p_y_int_min]
    cmp word [p2_x_int], ax
    jae .y_min_done ; new min if below current
    mov ax, word [p2_x_int]
    mov word [p_y_int_min], ax
.y_min_done: 


.p0_y_max:
    mov ax, word [p_y_int_max]
    cmp word [p0_y_int], ax
    jbe .p1_y_max ; new max if above current
    mov ax, word [p0_y_int]
    mov word [p_y_int_max], ax

.p1_y_max:
    mov ax, word [p_y_int_max]
    cmp word [p1_y_int], ax
    jbe .p2_y_max ; new max if above current
    mov ax, word [p1_y_int]
    mov word [p_y_int_max], ax

.p2_y_max
    mov ax, word [p_y_int_max]
    cmp word [p2_y_int], ax
    jbe .y_max_done ; new max if above current
    mov ax, word [p2_y_int]
    mov word [p_y_int_max], ax
.y_max_done: 


    ret
set_current_triangle_loop_span_end:


; ---   draw_current_triangle ------
;
;   1. sets triangle bounding box values (p_x_int_min, p_x_int_max, p_y_int_min, p_y_int_max)
;   2. loops through bounding box integer values
;   3. sets all three current triangle signed areas
;       4. draw pixel IFF all signed areas are positive (the front of triangle defined using right hand rule)
;
draw_current_triangle:
    push bp
    mov bp, sp

    call set_current_triangle_loop_span

    ; Loop through pixel row 150
    mov ax, word [p_x_int_min]
    mov word [p_x_int], ax   ; start col

    mov ax, word [p_y_int_min]
    mov word [p_y_int], ax ; start row

    mov ax, word [p_y_int_max]
    mov cx, ax ; last row
    .next_row:
    
    mov ax, word [p_x_int_min]
    mov word [p_x_int], ax   ; reset col to lowest x value of current triangle

    mov dx, word [p_x_int_max] ; last col
    .next_col:

    ; Update the float values to current integer indexes
    fild word [p_x_int]
    fstp dword [p_x]
    fild word [p_y_int]
    fstp dword [p_y]

    ; check if we should draw
    ; if all are POSITIVE, draw pixel!
    call calc_signed_area_0
    cmp ax, 1
    jne .end_draw
    call calc_signed_area_1
    cmp ax, 1
    jne .end_draw
    call calc_signed_area_2
    cmp ax, 1
    jne .end_draw
    

    mov bx, word [p_y_int]
    mov ax, word [p_x_int]
    call pixel_xa_yb

    .end_draw:

    inc word [p_x_int]
    cmp word [p_x_int], dx
    jb .next_col

    inc word [p_y_int]
    cmp word [p_y_int], cx
    jb .next_row

    mov sp, bp
    pop bp
    ret
draw_current_triangle_end:




;---- calc_signed_area_a to calc_signed_area_c -------
;
;   variables used: [fixed memory locations]
;       p_x, pa_x, pb_x
;       area_sign_int_a, area_sign_a
;
;   returns : ax = 1 if sign is positive
;
;   https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
;   
;   compare current point 'p' with line from p_a to p_b
;   (p_x - pb_x) * (pa_y - pb_y) - (pa_x - pb_x) * (p_y - pb_y)
;   = T1 - T2
; 
;   POSTFIX: 
;       T1 = p_x pb_x - pa_y pb_y - *
;       T2 = pa_x pb_x - p_y pb_y - *
;       
;       T1 - T2 = p_x pb_x - pa_y pb_y - * pa_x pb_x - p_y pb_y - * -
;
calc_signed_area_0:

    fld dword [p_x]
    fld dword [p1_x]
    fsubp
    fld dword [p0_y]
    fld dword [p1_y]
    fsubp
    fmulp

    fld dword [p0_x]
    fld dword [p1_x]
    fsubp
    fld dword [p_y]
    fld dword [p1_y]
    fsubp
    fmulp

    fsubp

    
    fist word [area_sign_int_0]
    fstp dword [area_sign_0]

    cmp word [area_sign_int_0], 0
    jl .negative
    mov ax, 1
    ret
    .negative:
    mov ax, 0
    ret
calc_signed_area_0_end:
calc_signed_area_1:

    fld dword [p_x]
    fld dword [p2_x]
    fsubp
    fld dword [p1_y]
    fld dword [p2_y]
    fsubp
    fmulp

    fld dword [p1_x]
    fld dword [p2_x]
    fsubp
    fld dword [p_y]
    fld dword [p2_y]
    fsubp
    fmulp

    fsubp

    
    fist word [area_sign_int_1]
    fstp dword [area_sign_1]

    cmp word [area_sign_int_1], 0
    jl .negative
    mov ax, 1
    ret
    .negative:
    mov ax, 0
    ret
calc_signed_area_1_end:
calc_signed_area_2:

    fld dword [p_x]
    fld dword [p0_x]
    fsubp
    fld dword [p2_y]
    fld dword [p0_y]
    fsubp
    fmulp

    fld dword [p2_x]
    fld dword [p0_x]
    fsubp
    fld dword [p_y]
    fld dword [p0_y]
    fsubp
    fmulp

    fsubp

    
    fist word [area_sign_int_2]
    fstp dword [area_sign_2]

    cmp word [area_sign_int_2], 0
    jl .negative
    mov ax, 1
    ret
    .negative:
    mov ax, 0
    ret
calc_signed_area_2_end:



;--- transform_triangle_1_into_current ---------
; 
;   Applies transformations and move into 'current' points
;   
transform_triangle_1_into_current:
    push bp
    mov bp, sp

    ; 1. Move triangle_1 points to current triangle
    ; 2. apply position transformation
 
    ; point 0
    ; x
    fld dword [triangle_1+0]
    fld dword [position + 0]
    faddp
    ; fistp word [current_triangle_pixels+0]
    ; fstp dword [current_triangle + 0]
    ; fstp dword [current_triangle + 0]
    fist word [p0_x_int]
    fstp dword [p0_x]
    ; y
    fld dword [triangle_1+4]
    fld dword [position + 4]
    faddp
    ; fistp word [current_triangle_pixels+2]
    ; fstp dword [current_triangle + 4]
    fist word [p0_y_int]
    fstp dword [p0_y]

    ; point 1

    fld dword [triangle_1+12]
    fld dword [position + 0]
    faddp
    ; fistp word [current_triangle_pixels+4]
    ; fstp dword [current_triangle + 12]
    fist word [p1_x_int]
    fstp dword [p1_x]

    fld dword [triangle_1+16]
    fld dword [position + 4]
    faddp
    ; fistp word [current_triangle_pixels+6]
    ; fstp dword [current_triangle + 16]
    fist word [p1_y_int]
    fstp dword [p1_y]

    ; point 2
    fld dword [triangle_1+24]
    fld dword [position + 0]
    faddp
    ; fistp word [current_triangle_pixels+8]
    ; fstp dword [current_triangle + 24]
    fist word [p2_x_int]
    fstp dword [p2_x]

    fld dword [triangle_1+28]
    fld dword [position + 4]
    faddp
    ; fistp word [current_triangle_pixels+10]
    ; fstp dword [current_triangle + 28]
    fist word [p2_y_int]
    fstp dword [p2_y]

    ; call draw_current_triangle_points


    mov sp, bp
    pop bp
    ret
transform_triangle_1_into_current_end:



current_triangle_into_pixel_coord:
    push bp
    mov bp, sp

    ; 1. Move current triangle points to pixel coordinates
 
    ; point 0
    ; x
    fld dword [p0_x]
    fistp word [current_triangle_pixels+0]
    ; y
    fld dword [p0_y]
    fistp word [current_triangle_pixels+2]

    ; point 1
    ; x
    fld dword [p1_x]
    fistp word [current_triangle_pixels+4]
    ; y
    fld dword [p1_y]
    fistp word [current_triangle_pixels+6]

    ; point 2
    ; x
    fld dword [p2_x]
    fistp word [current_triangle_pixels+8]
    ; y
    fld dword [p2_y]
    fistp word [current_triangle_pixels+10]

    ; call draw_current_triangle

    mov sp, bp
    pop bp
    ret
current_triangle_into_pixel_coord_end:



draw_current_triangle_points:
    mov si, current_triangle_pixels

    mov ax, [si+0]
    mov bx, [si+2]
    call pixel_xa_yb

    mov ax, [si+4]
    mov bx, [si+6]
    call pixel_xa_yb

    mov ax, [si+8]
    mov bx, [si+10]
    call pixel_xa_yb

    ret
draw_current_triangle_points_end:




;--- draw_sine ---------
; 
;   Loops through integers in range [0, SCREEN_WIDTH].
;   Maps each x-coord to sine function.
;   Will print the y-coord
draw_sine:
    push bp
    mov bp, sp

    ; Local vars
    sub sp, 2 ; x = [bp - 2]
    sub sp, 2 ; y = [bp - 4]
    mov word [bp - 2], 0

    sub sp, 2 ; A = [bp - 6]
    sub sp, 2 ; V = [bp - 8]
    sub sp, 2 ; c = [bp - 10]
    mov word [bp - 6], 20
    mov word [bp - 8], 100
    mov word [bp - 10], 10
    
    ; Loop through all x-coords
    .next_x:

    ;-------------------------
    ;   Sine curve
    ;   f(x) =  V + A*sin(x/c)
    ;   RPN:    V A x c / sin * +
    ;------------------------]
    fild word [bp - 8]  ; V
    fild word [bp - 6]  ; A
    fild word [bp - 2]  ; x
    fild word [bp - 10] ; c
    fdivp
    fsin
    fmulp
    faddp

    fistp word [bp - 4] ; y
    
    ; Draw pixel
    mov ax, word [bp - 2]
    mov bx, word [bp - 4]
    call pixel_xa_yb


    inc word [bp - 2]
    cmp word [bp - 2], 320
    jb .next_x

    mov sp, bp
    pop bp
    ret
draw_sine_end:
    
; First attempt to reduce flickering by swapping
swap_buffer:
    ; pusha

    ; fsqrt

    ; draw six pixels to second buffer
    mov ax, 0x7000
    mov es, ax
    mov di, 642
    mov word [es:di], 0x0F0F
    mov di, 644
    mov word [es:di], 0x0F0F
    mov di, 646
    mov word [es:di], 0x0F0F
    mov di, 648
    mov word [es:di], 0x0F0F
    mov di, 650
    mov word [es:di], 0x0F0F

    ; cli
    push ds

    call wait_for_vsync
    
    mov ax, 0x7000
    mov es, ax

    mov ax, 0xA000
    mov ds, ax

    xor di, di
    xor si, si
    mov cx, 64000
    mov dx, 64000
    ; xor cx, cx
    ; xor si, si
    .loop_1:

    mov al, byte [es:si]
    mov byte [ds:di], al

    ; rep movsw
    inc si
    inc di
    ; sli
    cmp si, dx
    jb .loop_1
    ; DEBUG
    ; cli
    ; hlt

    pop ds



    ; popa
    ret
swap_buffer_end:

; Attempt to avoid writing to framebuffer during render
wait_for_vsync:
    mov dx, 0x03DA     ; VGA input status register
vsync_start:
    in al, dx          ; Read the status
    test al, 0x08      ; Check vertical retrace bit (bit 3)
    jz vsync_start     ; Wait until retrace starts
vsync_end:
    in al, dx
    test al, 0x08
    jnz vsync_end      ; Wait until retrace ends
    ret

; Draws a triangle outline based on the 'tri_2d_int_array' array
;
; No inputs nor outputs; only reading the fixed data array
draw_triangle:
    ; pusha
    push bp
    mov bp, sp

    ; P1 : Upper left corner
    mov bx, word [tri_2d_int_array+2]    ; y
    mov ax, word [tri_2d_int_array+0]    ; x
    call pixel_xa_yb
    ; add sp, 6

    ; P2 : top right corner
    mov bx, word [tri_2d_int_array+8]    ; y
    mov ax, word [tri_2d_int_array+6]    ; x
    call pixel_xa_yb

    ; P3 : Bottom left corner
    mov bx, word [tri_2d_int_array+14]    ; y
    mov ax, word [tri_2d_int_array+12]    ; x
    call pixel_xa_yb


    ;
    ; Draw line from point 1 to point 2
    ;
    push word [tri_2d_int_array+8] ; y_1
    push word [tri_2d_int_array+6] ; x_1
    push word [tri_2d_int_array+2] ; y_0
    push word [tri_2d_int_array+0] ; x_0
    call draw_line
    add sp, 8
    ffree st1
    ;
    ; Draw line from point 1 to point 3
    ;
    push word [tri_2d_int_array+14] ; y_1
    push word [tri_2d_int_array+12] ; x_1
    push word [tri_2d_int_array+2] ; y_0
    push word [tri_2d_int_array+0] ; x_0
    call draw_line
    add sp, 8

    ;
    ; Draw line from point 3 to point 2
    ;


    ; DELTAS
    ;
    ; delta x
    mov ax, [tri_2d_int_array+6]
    sub ax, [tri_2d_int_array+12]
    ; delta y
    mov bx, [tri_2d_int_array+8]
    sub bx, [tri_2d_int_array+14]

    ; Add delta as constants for the rest of point 3 to point 2
    push ax
    push bx
    

    ; SLOPE
    ;
    mov ax, word [bp - 2]
    mov bx, word [bp - 4]
    call slope_100
    ; slope returned in dx
    push dx ; persist the slope100
    mov di, [bp - 6]

    ; print slope
    push ax
    mov ax, dx
    call print_hex_value
    pop ax
    ; draw points between point 3 and 2
    ; starting at point 3

    ; push ax ; delta x

    mov cx, 0; x index
    .line_next_step:
    inc cx
    
    ; starting values
    mov ax, word [tri_2d_int_array+12] ; x
    mov bx, word [tri_2d_int_array+14] ; y

    ; increment x coord
    add ax, cx

    ; multiply x offset by slope
    push cx ; [pb - 8] = x offset from origin point
    fild word [bp - 8]
    fild word [bp - 6] ; slope100
    fmul
    add sp, 2 ; remove x offset
    push word 100 ; [pb - 8] = slope multiplier
    fild word [bp - 8]
    fdiv
    fistp word [bp - 8]
    mov dx, word [bp - 8]
    add sp, 2     ; clear slope multiplier



    add bx, dx

    call pixel_xa_yb

    cmp cx, [bp - 2]
    jle .line_next_step

    
    
    add sp, 6

    

    mov sp, bp
    pop bp
    ; popa
    ret
draw_triangle_end:



;   fn: DRAW_LINE
;
;   y_1 = bp + 10
;   x_1 = bp + 8
;   y_0 = bp + 6
;   x_0 = bp + 4
;
draw_line:
    push bp
    mov bp, sp

    ; DELTAS
    ;
    ; delta x
    mov ax, [bp + 8]
    sub ax, [bp + 4]
    ; delta y
    mov bx, [bp + 10]
    sub bx, [bp + 6]

    ; Add delta as constants for the rest of point 3 to point 2
    push ax
    push bx
    

    ; SLOPE
    ;
    mov ax, word [bp - 2]
    mov bx, word [bp - 4]
    call slope_100
    ; slope returned in dx
    push dx ; persist the slope100
    mov di, [bp - 6]

    ; print slope
    ; push ax
    ; mov ax, dx
    ; call print_hex_value
    ; pop ax
    ; draw points between point 3 and 2
    ; starting at point 3

    ; push ax ; delta x

    mov cx, 0; x index
    .line_next_step:
    inc cx
    
    ; starting values
    mov ax, word [bp + 4] ; x
    mov bx, word [bp + 6] ; y

    ; increment x coord
    add ax, cx

    ; multiply x offset by slope
    push cx ; [pb - 8] = x offset from origin point
    fild word [bp - 8]
    fild word [bp - 6] ; slope100
    fmul
    add sp, 2 ; remove x offset
    push word 100 ; [pb - 8] = slope multiplier
    fild word [bp - 8]
    fdiv
    fistp word [bp - 8]
    mov dx, word [bp - 8]
    add sp, 2     ; clear slope multiplier



    add bx, dx

    call pixel_xa_yb

    cmp cx, [bp - 2]
    jle .line_next_step

    
    
    add sp, 6

    mov sp, bp
    pop bp
    ret

draw_line_end:



; fn: PIXEL_XA_YB
;
; Draws pixel WITHOUT register or stack side effects.
; Color: white
; Coordinate system: right handed with origin botton left
; input:
;       ax : x location
;       bx : y location
pixel_xa_yb:
    push cx ; x index
    push dx ; y index
    push es ; video memory segment
    push di ; offset

    mov cx, VIDEO_D_BUFFER ; cx is a temp leave ax/bx untouched
    mov es, cx
    xor cx, cx

    mov dx, 200
    sub dx, bx

    mov cx, ax

    mov di, 320
    imul di, dx
    add di, cx

    mov byte [es:di], 0x0F
    
    pop di
    pop es
    pop dx
    pop cx
    ret
pixel_xa_yb_end:





; fn    :   Returns the slope of the delta x & y.
; in    :   ax = delta x
;           bx = delta y
; ret   :   dx = slope * 100 (dy/dx*100)
; modreg:   
slope_100:
    push bp
    mov bp, sp

    push ax     ; delta x
    push bx     ; delta y
    push word 100
    sub sp, 2   ; slope integer

    fild word [bp - 4] ; dy = ST 1
    fild word [bp - 2] ; dx = ST 0

    fdiv  ; ST(0) = ST(1) / ST(0) ?

    fild word [bp - 6] ; 100 multiplier

    fmul

    ; fistp dword [slope_float]     ; Convert and pop ST(0)

    ; pop into memory, then into ax for resturn
    fistp word [bp - 8]
    mov dx, word [bp - 8]   

    
    add sp, 2 ;reset local variable
    add sp, 2 ;
    pop bx ; delta y
    pop ax ; delta x

    mov sp, bp
    pop bp
    ret
slope_100_end:

; fn    :   Returns the slope of the delta x & y.
; in    :   ax = delta x
;           bx = delta y
; ret   :   si = slope * 100 (dy/dx*100)
; modreg:   
slope_calc_100_old_int:
    push cx
    push dx

    mov si, 0x0000
        mov dx, bx
        imul dx, 100
        ; count number of dx in dy*100
        mov cx, 0
    .slope_calc_loop:
        add cx, ax
        inc si
        cmp cx, dx
        jle .slope_calc_loop
    .slope_calc_loop_end:

    pop dx
    pop cx
    ret
slope_calc_100_old_int_end:



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
    push 0x02   ; color
    push word [player_position_y]    ; y
    push word [player_position_x]    ; x
    call draw_2x2
    add sp, 6
    ret


; fn: prints the 16-bit value as hex number in screen low right corner
; in: ax = word-width value to print
print_hex_value:
    push bp
    mov bp, sp

    ; Store the original value to be printed
    push ax


    ; word_l, low nibble
    mov si, [bp - 2]
    shl si, 12
    shr si, 12
    mov bl, byte [hex_print_table + si]
    xor bh, bh

    mov ax, bx
    call char_ascii_to_bitmap_address
    push ax             ; char address
    push 190            ; y
    push 310             ; x
    call write_char_from_bitmap_address
    add sp, 6


    ; word_l, high nibble
    mov si, [bp - 2]
    shl si, 8
    shr si, 12
    mov bl, byte [hex_print_table + si]
    xor bh, bh

    mov ax, bx
    call char_ascii_to_bitmap_address
    push ax             ; char address
    push 190            ; y
    push 300             ; x
    call write_char_from_bitmap_address
    add sp, 6

    ; word_h, low nibble
    mov si, [bp - 2]
    shl si, 4
    shr si, 12
    mov bl, byte [hex_print_table + si]
    xor bh, bh

    mov ax, bx
    call char_ascii_to_bitmap_address
    push ax             ; char address
    push 190            ; y
    push 290             ; x
    call write_char_from_bitmap_address
    add sp, 6

    ; word_h, high nibble
    mov si, [bp - 2]
    shl si, 0
    shr si, 12
    mov bl, byte [hex_print_table + si]
    xor bh, bh

    mov ax, bx
    call char_ascii_to_bitmap_address
    push ax             ; char address
    push 190            ; y
    push 280             ; x
    call write_char_from_bitmap_address
    add sp, 6


    pop ax

    mov sp, bp
    pop bp
    ret
print_hex_value_end:



print_available_chars:

    mov ax, '0'
    call char_ascii_to_bitmap_address
    push ax             ; char address
    push 190            ; y
    push 10             ; x
    call write_char_from_bitmap_address
    add sp, 6


    mov ax, '1'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 20             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '2'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push word 190           ; y
    push word 30            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '3'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 40             ; x
    call write_char_from_bitmap_address
    add sp, 6

     mov ax, '4'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 40             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '5'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push word 190           ; y
    push word 50            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '6'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 60             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '6'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 70             ; x
    call write_char_from_bitmap_address
    add sp, 6

     mov ax, '7'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 80             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '8'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push word 190           ; y
    push word 90            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, '9'
    call char_ascii_to_bitmap_address
    push ax          ; Letter address
    push 190            ; y
    push 100             ; x
    call write_char_from_bitmap_address
    add sp, 6


    mov ax, 'A'
    call char_ascii_to_bitmap_address
    ; mov si, ax          ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 110             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'B'
    call char_ascii_to_bitmap_address
    ; mov si, char_B          ; Letter address
    push ax          ; Letter address
    push word 190           ; y
    push word 120            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'C'
    call char_ascii_to_bitmap_address
    mov si, char_C      ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 130             ; x
    call write_char_from_bitmap_address
    add sp, 6

     mov ax, 'D'
    call char_ascii_to_bitmap_address
    ; mov si, ax          ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 140             ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'E'
    call char_ascii_to_bitmap_address
    ; mov si, char_B          ; Letter address
    push ax          ; Letter address
    push word 190           ; y
    push word 150            ; x
    call write_char_from_bitmap_address
    add sp, 6

    mov ax, 'F'
    call char_ascii_to_bitmap_address
    mov si, char_C      ; Letter address
    push ax          ; Letter address
    push 190            ; y
    push 160             ; x
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

.0: cmp ax, 0x30
    jne .1
    mov ax, char_0
    jmp .done

.1: cmp ax, 0x31
    jne .2
    mov ax, char_1
    jmp .done

.2: cmp ax, 0x32
    jne .3 
    mov ax, char_2
    jmp .done

.3: cmp ax, 0x33
    jne .4 
    mov ax, char_3
    jmp .done

.4: cmp ax, 0x34
    jne .5 
    mov ax, char_4
    jmp .done

.5: cmp ax, 0x35
    jne .6
    mov ax, char_5
    jmp .done

.6: cmp ax, 0x36
    jne .7
    mov ax, char_6
    jmp .done

.7: cmp ax, 0x37
    jne .8
    mov ax, char_7
    jmp .done

.8: cmp ax, 0x38
    jne .9 
    mov ax, char_8
    jmp .done

.9: cmp ax, 0x39
    jne .A
    mov ax, char_9
    jmp .done

.A: cmp ax, 0x41
    jne .B
    mov ax, char_A
    jmp .done

.B: cmp ax, 0x42
    jne .C
    mov ax, char_B
    jmp .done

.C: cmp ax, 0x43
    jne .D 
    mov ax, char_C
    jmp .done

.D: cmp ax, 0x44
    jne .E 
    mov ax, char_D
    jmp .done

.E: cmp ax, 0x45
    jne .F 
    mov ax, char_E
    jmp .done

.F: cmp ax, 0x46
    jne .default ; NOTE THE CUSTOM END OF SWITCHING!
    mov ax, char_F
    jmp .done

.default:
    mov ax, char_default
    jmp .done

.done:
    ret
char_ascii_to_bitmap_address_end:

; routine is heavily commented for the sake of learning!
write_char_from_bitmap_address:
    push bp
    mov bp, sp

    ; char bitmap address
    mov si, [bp+8]

    ; set up buffer segment
    mov ax, VIDEO_D_BUFFER
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
write_char_from_bitmap_address_end:



; @ x [i16] - bp + 4
; @ y [i16] - bp + 6
; @ c [i16] - bp + 8
draw_2x2:

    ; save previous stack frame. Then set up new one. 
    push bp
    mov bp, sp ; can't use stack pointer for effective address calculation
    
    mov ax, VIDEO_D_BUFFER         ; Segment for video memory
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
    mov ax, VIDEO_D_BUFFER         ; Segment for graphics video memory
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
    mov ax, VIDEO_D_BUFFER
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

    mov ax, VIDEO_D_BUFFER         ; Segment for video memory
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
    
    mov ax, VIDEO_D_BUFFER         ; Segment for video memory
    mov es, ax             ; Point ES to video memory

    mov di, (320 * 51 + 51) ; Offset for pixel at (100, 50)
    mov al, 0x07           ; Pixel color (bright white)

    mov [es:di], al        ; Write pixel color to video memory

    ret
