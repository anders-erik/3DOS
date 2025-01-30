section .data
; align 4
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

cursor_buffer times 256 db 0
ascii_current_press dw 0
cursor_count dw 0
cursor_c dw 0
cursor_r dw 19
cursor_w dw 10
cursor_h dw 10

