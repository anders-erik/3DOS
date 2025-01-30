; NOTE:
;   The data below is placed in this separate file for performance reasons.
;   When previously placed interleaved between .text section this cpu usage went up significantly. 


section .data
; Double buffering segment
VIDEO_D_BUFFER equ 0x7000


tri_2d_int_array dw 20, 60, 0, 150, 80, 0, 120, 30, 0
slope_int dw 0
slope_float dd 0

; test_var dw 1000

; Testing floating point numbers
float_1 dd 5.14
float_2 dd 5.71
float_3 dd 20.22
float_res dd 0.0
integer_res dw 0


; Will hold the triangle points to be drawn next
; Expressed in screen coordinates
; 12 bytes
current_triangle_pixels dw  140, 140, 190 ,140 ,140 ,190    ; x0, y0, x1, y1, x2, y2
; 24 bytes          ; x0, y0 &  x1, y1 & x2, y2
current_triangle    dd      60.0,   60.0,  \
                            110.0,  60.0,  \
                            60.0,   110.0,

; Current, transformed, window coord, float triangle
; Used for triangle drawing
p0_x dd 0.0
p0_y dd 0.0
p1_x dd 0.0
p1_y dd 0.0
p2_x dd 0.0
p2_y dd 0.0
p0_x_int dw 0.0
p0_y_int dw 0.0
p1_x_int dw 0.0
p1_y_int dw 0.0
p2_x_int dw 0.0
p2_y_int dw 0.0




; Signed area values
;
; current pixel location for discrete pixel loop
p_x_int dw 0
p_y_int dw 0
; current triangle loop range
p_x_int_min dw 0
p_x_int_max dw 0
p_y_int_min dw 0
p_y_int_max dw 0
; current pixel as float for signed area calcs
p_x dd 0
p_y dd 0
; Store the signed area calculation to determine if pixel is within triangle, thus if it is to be drawn
area_sign_0 dd 0
area_sign_1 dd 0
area_sign_2 dd 0
area_sign_int_0 dw 0
area_sign_int_1 dw 0
area_sign_int_2 dw 0

