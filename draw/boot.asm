;   
;   A raw binary that renders to a qemu vm in real mode
;

; RUN
;
; // source : Claude
; nasm -f bin -o draw.bin boot.asm
; qemu-system-i386 -drive format=raw,file=draw.bin 

; This one from GPT also works! Probably the same BIOS?
; qemu-system-x86_64 -drive format=raw,file=draw.bin // GPT

; DISASSEMBLE
; objdump -b binary -m i386 -D draw.bin



%ifdef ELF
section .text
global _start
_start:
%else
[bits 16]
[org 0x7c00]
%endif

; variable for boot sector playground
boot_x dw 0 ; bugs if defined within boot_sector section



; Boot sector - ran out of boot sector so had to move code to second_sector
boot_sector:


    ; Basic setup
    cli
    ; mov ax, 0x0000
    ; mov cs, ax ; Why doesn't this work? I thought that cs=0 by default, so this should change nothing?
    mov ax, 0x0000
    mov ds, ax
    ; mov ax, 0x0000
    ; mov es, ax
    mov ss, ax
    mov sp, 0x7C00

    ; additional segment registers
    ; NOT currently in use because they did not exists pre 80386
    mov ax, 0x0000
    mov fs, ax
    mov gs, ax
    sti

    ; Load second stage     : sector # 2-5 = 0x7e00 - 0x85FF
    mov ah, 0x02            ; BIOS read sector function
    mov al, 8               ; Number of sectors to read -- INCREASING THIS WAS CRUCIAL IN MOVING BEYOND BOOT SECTOR
    mov ch, 0               ; Cylinder number
    mov cl, 2               ; Sector number (1 is boot sector)
    mov dh, 0               ; Head number
    mov dl, 0x80            ; Drive number (first hard disk)
    mov bx, code_segment    ; Where to load the sector
    int 0x13                ; BIOS interrupt to read disk

    ; Load data             : sector # 6   = 0x0x8600 - 0x87FF
    ; This section will automatically be
    mov ah, 0x02            ; BIOS read sector function
    mov al, 2               ; Number of sectors to read
    mov ch, 0               ; Cylinder number
    mov cl, 10              ; Sector number (2-9 is code)
    mov dh, 0               ; Head number
    mov dl, 0x80            ; Drive number (first hard disk)
    ; mov ax, 0x0000
    ; mov es, ax          ; segment
    mov bx, 0x8E00          ; make sure data sectors at the end of the main text sections 
    int 0x13                ; BIOS interrupt to read disk
    
    
    

    ;;
    ;; START - Boot section playground
    ;;

.mode_13h_and_clear:
    mov ax, 0x13
    int 0x10

    mov ah, 0x06    ; Scroll up function
    mov al, 0       ; Clear entire screen
    mov bh, 0x0C    ; Light Red 
    mov ch, 0       ; Upper left row
    mov cl, 0       ; Upper left column
    mov dh, 24      ; Lower right row
    mov dl, 79      ; Lower right column
    int 0x10        ; Call BIOS video interrupt

    ; Landing buffer
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000
    mov ax, 0x0000


    ; draw pixel
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0F  ; White color
    xor cx, cx
    xor dx, dx
    inc word [boot_x]
    cmp word [boot_x], 6400 ; number of iterations and pixels draw until progressing past boot sector
    jge .load_code_segment
    mov cx, [boot_x]  ; x
    mov dx, 10  ; y
    int 0x10

    ; jump back into landing buffer
    ; jmp 0x0000:0x7C25
    jmp 0x0000:0x7C60
    ; jmp 0x0000:0x7C70

    ;;
    ;; END - Boot section playground
    ;;


    ; mov al, 0x00       ; Exit QEMU with status 0
    ; out 0x501, al      ; Write to QEMU's debug exit port

    .load_code_segment:
    
    ; jump to segment 2
    ; jmp 0x0000:0x7E00
    jmp code_segment



%ifndef ELF
times 510-($-$$) db 0
dw 0xaa55
%endif











section .text
; --------------------------------------------------------
;  CODE SEGMENT @ 0x7e00 (0x7c00 + 0xFF boot sector size)

code_segment:


; Interrupt Data
PIT_CONTROL_PORT  equ 0x43
PIT_CHANNEL_0     equ 0x40
IRQ0_VECTOR       equ 0x08

; used for restoring irq0 state -- implementation in 'timer_interrupt.asm'
old_irq0_offset dw 0x0000
old_irq0_segment dw 0x0000
; PROBLEM WHEN INCLUDING THESE VARIABLES IN TEXT SEGMENT!
; Uncommenting the blow definition will be interpereted as an instruction rather than data, blocking the vm!
; old_irq0_segment dw 0x8bfa 

; timing interrupt counter
tick_count dw 0




; Set up video mode (320x200, 256 colors)
mov ax, 0x13
int 0x10

; Set up keyboard interrupt handler
cli
mov word [0x24], keyboard_handler
mov word [0x26], 0
sti

; mov ax, old_irq0_offset
; mov bx, ds
; mov ax, word [bx]


; Figure out location of word_array, as it is NOT in this location when going through disassembled code
; -->  00000219:  A30086  mov [0x8600],ax
; Turns out that all '.code' sections will be concatenated to the assembled .text sections, and the IP will not run the instructions placed in the .code!
; 
mov word [word_array], ax

section .data


; ; NEEDED TO DEFINE THE ARRAY HERE WITHOUT A SECTION-LABEL TO BE ABLE TO USE THE ARRAY!
word_array: dw 10, 20, 30, 40, 50 ; An array of 5 bytes
; ; word_buffer: resw 5 ; 10 bytes without specifying initial value

; byte_array: times 10 db 0 ; 10 bytes initaliezed to '0'
; byte_array_size: equ 10

; frame_buff: times 64 db 0 ; Cant use the 64k that I want for a full framebuffer..
; ; frame_buff_size: equ 640

; framebuffer_64k: 0x7E00
; Initialize buffer location
    mov ax, 0x7E00     ; Address right after boot sector
    mov es, ax         ; Set extra segment
    xor di, di         ; Zero offset
    mov cx, 10000        ; Number of bytes to clear
    xor al, al         ; Value to fill (zero)
    rep stosb          ; Clear the buffer



; PIXEL COORIDINATES
pixel_x dw 0
pixel_y dw 0
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

; player location
x dw 0
y dw 0
mov WORD [y], 0
mov WORD [x], 0

; player position : NOTE: INTIAL VALUE WILL ALWAYS BE 0 WHEN DEFINED IN .data!
player_position_x dw 0xA0
player_position_y dw 0x60
; mov word [player_position_x], 0xA0
; mov word [player_position_y], 0x60

call render
call render
call render
call render
call render


section .text


; ; player position
; player_position_x dw 0xA0
; player_position_y dw 0x60

timer_setup:
 ; timer interrupt
    cli

    ; Set up PIT for periodic interrupts
    mov al, 0x36                ; Control word: Mode 3, Square wave generator
    out PIT_CONTROL_PORT, al
    ; mov ax, 1193180        ; Set frequency to 100 Hz (adjust as needed)

    ; minimum : https://en.wikibooks.org/wiki/X86_Assembly/Programmable_Interval_Timer
    mov ax, 65535 ; ( ~= 1193180 / 18.2)
    out PIT_CHANNEL_0, al       ; Low byte
    mov al, ah
    out PIT_CHANNEL_0, al       ; High byte


    ; Set up custom IRQ0 handler
    ; Save old handler address
    mov ax, 0x0000
    mov es, ax                  ; ES points to interrupt vector table
    mov di, IRQ0_VECTOR * 4
    ; backup already existing values in table
    mov ax, [es:di]        ; Load offset into AX
    mov [old_irq0_offset], ax
    mov ax, [es:di+2]      ; Load segment into AX
    mov [old_irq0_segment], ax

    ; Set new handler
    ; cli
    mov word [es:di], timer_handler   ; Offset of custom handler
    mov word [es:di+2], cs      ; Segment of this code
    sti                         ; Re-enable interrupts
timer_setup_end:




; Main loop
; Stack is untouched at this point
main_loop:
    hlt
    jmp main_loop





timer_handler:
    pusha
    inc word [tick_count]

    mov bp, sp ; bp already pushed duing hanlder entry

    call render

    call update


    ; PROBLEM: LOCKS IN BOOT SECTOR AND BLASTS CPU USAGE!
    ; Render white pixel
    ; mov ax, 0xA000         ; Segment for video memory
    ; mov es, ax             ; Point ES to video memory
    ; mov di, [tick_count] ; Offset for pixel at (100, 50)
    ; ; mov di, 50 * 320 + 100 ; Offset for pixel at (100, 50)
    ; mov al, 0x0F           ; Pixel color (bright white)
    ; mov [es:di], al        ; Write pixel color to video memory

    ; cmp WORD [w_pressed], 1
    ; je .w_not_pressed
    ; ; mov ah, 0x0C  ; BIOS video function: write pixel
    ; ; mov al, 0x01  ; color -- Blue 
    ; xor cx, cx
    ; xor dx, dx
    ; ; mov cx, 60
    ; ; mov dx, 60

    ; ; int 0x10
    ; .w_not_pressed:


    ; make sure this label is reachable
    ; Renders a pixel at [tick_count]
    ; call reachable
    ; call extern_pixels
    ; call write_oooo

    

    ; EOI command making sure proper end of interrupt?
    mov al, 0x20                ; EOI command
    out 0x20, al                ; Send to PIC command port (0x20)

    ; End of handler
    popa                       ; Restore all registers
    iret                      ; Return from interrupt




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


keyboard_handler_done:
    mov al, 0x20
    out 0x20, al  ; Send EOI to PIC
    popa
    iret ; interrupt - meaning : 2024-10-31

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









update:

.update_player_position:

.w: cmp word [w_pressed], 1
    jne .a
    sub word [player_position_y], 2
    
.a: cmp word [a_pressed], 1
    jne .s
    sub word [player_position_x], 2

.s: cmp word [s_pressed], 1
    jne .d
    add word [player_position_y], 2

.d: cmp word [d_pressed], 1
    jne .update_player_position_end
    add word [player_position_x], 2

.update_player_position_end:


; Update current pixel position
.update_location:
    inc word [pixel_x]
    cmp word [pixel_x], 320
    ret
    mov word [pixel_x], 0
    inc word [pixel_y]
    cmp word [pixel_y], 200
    ret 
    mov word [pixel_y], 0
    ; ret

    ret

.update_location_end:



reachable:
    pusha
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0F  ; White color
    mov cx, [tick_count]
    mov dx, 20
    int 0x10
    popa
    ret


float_test:

    finit

    ; Load into FPU stack
    ; top of stack == ST(0)
    fld dword [float_1]    ; ST(0) = float_1
    fld dword [float_2]    ; ST(0) = float_2, ST(1) = float_1
    fld dword [float_3]    ; ST(0) = float_3, ST(1) = float_2, ST(2) = float_1

    ; what happens if stack is filled?
    fild word [tri_2d_int_array]


    ; fxch ; Swaps spot 0 and 1 ??

    ; operations
    ; fadd                  ; ST(0) = ST(0) + ST(1)
    ; fadd ST(1)     ; ST(0) = ST(0) + ST(1)
    ; fadd
    fmul                  ; ST(1) = ST(0) * ST(1)
    ; fdiv                    ; ST(1) = ST(0) / ST(1)

    ; ; pop the FPU stack into memory
    ; fstp dword [float_res]
    

    ; ; push/load from memory onto FPU stack
    ; fld dword [float_res]   ; Load 'result' back into ST(0)

    ; Convert to integer and store in 'integer_result'
    fistp word [integer_res] ; Convert and pop ST(0)

    ; Clear stack?
    ; Realistically does nothing..
    fstp
    fstp

    ; Draw pixel
    mov ax, 0xA000
    mov es, ax
    mov di, word [integer_res]
    mov word [es:di], 0x0F0F
    ret


; C-like includes
%include "./draw/render.asm"
%include "./draw/keyboard.asm"
code_segment_end:

times 4096-(code_segment_end - code_segment) db 0



section .data
; %include "./draw/data.asm"
; my_data equ 0x0900
my_data:
tri_2d_int_array dw 100, 80, 0, 150, 80, 0, 140, 10, 0
slope_float dd 0
slope_int dw 0

test_var dw 1000

; Testing floating point numbers
float_1 dd 5.14
float_2 dd 5.71
float_3 dd 20.22
float_res dd 0.0
integer_res dw 0




char_0    db 01111110b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 01111110b

char_1    db 00011000b
          db 00011000b
          db 00011000b
          db 00011000b
          db 00011000b
          db 00011000b
          db 00011000b
          db 00011000b

char_2    db 11111111b
          db 00000001b
          db 00000001b
          db 11111111b
          db 10000000b
          db 10000000b
          db 10000000b
          db 11111111b

char_3    db 11111110b
          db 00000001b
          db 00000001b
          db 00011111b
          db 00000001b
          db 00000001b
          db 00000001b
          db 11111110b

char_4    db 10000001b
          db 10000001b
          db 10000001b
          db 11111111b
          db 00000001b
          db 00000001b
          db 00000001b
          db 00000001b

char_5    db 11111111b
          db 10000000b
          db 10000000b
          db 11111111b
          db 00000001b
          db 00000001b
          db 00000001b
          db 11111111b

char_6    db 11111111b
          db 10000000b
          db 10000000b
          db 10000000b
          db 11111111b
          db 10000001b
          db 10000001b
          db 11111111b

char_7    db 11111111b
          db 00000001b
          db 00000001b
          db 00000001b
          db 00000001b
          db 00000001b
          db 00000001b
          db 00000001b

char_8    db 11111111b
          db 10000001b
          db 10000001b
          db 11111111b
          db 10000001b
          db 10000001b
          db 10000001b
          db 11111111b

char_9    db 11111111b
          db 10000001b
          db 10000001b
          db 11111111b
          db 00000001b
          db 00000001b
          db 00000001b
          db 01111110b


char_A    db 00011000b
          db 00100100b
          db 01000010b
          db 01000010b
          db 01111110b
          db 01000010b
          db 01000010b
          db 01000010b

char_B    db 11111110b
          db 10000010b
          db 10000010b
          db 11111110b
          db 10000001b
          db 10000001b
          db 10000001b
          db 11111111b

char_C    db 11111111b
          db 10000001b
          db 10000000b
          db 10000000b
          db 10000000b
          db 10000000b
          db 10000001b
          db 11111111b

char_D    db 11111110b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 10000001b
          db 11111110b

char_E    db 11111111b
          db 10000000b
          db 10000000b
          db 11111100b
          db 10000000b
          db 10000000b
          db 10000000b
          db 11111111b

char_F    db 11111111b
          db 10000000b
          db 10000000b
          db 11111100b
          db 10000000b
          db 10000000b
          db 10000000b
          db 10000000b

char_default  db 10101010b
          db 10101010b
          db 10101010b
          db 10101010b
          db 10101010b
          db 10101010b
          db 10101010b
          db 10101010b

my_data_end:
; times 512-(my_data_end - my_data) db 0




; GPT - didn't work!
; org 0x7C00              ; The BIOS loads the bootloader at address 0x7C00
; ;;;
; start:
;     ; Set VGA mode 13h (320x200, 256 colors)
;     mov ax, 0x0013
;     int 0x10

;     ; Set ES segment to 0xA000 (start of VGA video memory)
;     mov ax, 0xA000
;     mov es, ax

;     ; Draw pixels in the top-left corner
;     xor di, di          ; Start at video memory offset 0
;     mov cx, 50          ; Draw 50 pixels in total

; draw_pixels:
;     mov al, 0x0F        ; Color 0x0F (white)
;     stosb               ; Store AL at ES:[DI], increment DI
;     loop draw_pixels

;     ; Halt the CPU
;     cli
;     hlt

; times 510-($-$$) db 0   ; Fill the rest of the boot sector with zeros
; dw 0xAA55               ; Boot signature


; [BITS 16]
; absolute 0x1000  ; Define absolute addressing from 0x1000
; absolute_label:

;     jmp 0x0000:0x7C00 ; Infinite loop
