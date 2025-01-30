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

section .text

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
    mov es, ax
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
    mov al, 4               ; Number of sectors to read
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


; section .text


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
    ; mov ax, 39773 ; ( ~= 1193180 / 30 )
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


    ; EOI command making sure proper end of interrupt?
    mov al, 0x20                ; EOI command
    out 0x20, al                ; Send to PIC command port (0x20)

    ; End of handler
    popa                       ; Restore all registers
    iret                      ; Return from interrupt




update:
    call .update_player_position
    call .update_location
    ret


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

    ret


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
.update_location_end:
    ret





; float_test:

;     finit

;     ; Load into FPU stack
;     ; top of stack == ST(0)
;     fld dword [float_1]    ; ST(0) = float_1
;     fld dword [float_2]    ; ST(0) = float_2, ST(1) = float_1
;     fld dword [float_3]    ; ST(0) = float_3, ST(1) = float_2, ST(2) = float_1

;     ; what happens if stack is filled?
;     fild word [tri_2d_int_array]


;     ; fxch ; Swaps spot 0 and 1 ??

;     ; operations
;     ; fadd                  ; ST(0) = ST(0) + ST(1)
;     ; fadd ST(1)     ; ST(0) = ST(0) + ST(1)
;     ; fadd
;     fmul                  ; ST(1) = ST(0) * ST(1)
;     ; fdiv                    ; ST(1) = ST(0) / ST(1)

;     ; ; pop the FPU stack into memory
;     ; fstp dword [float_res]
    

;     ; ; push/load from memory onto FPU stack
;     ; fld dword [float_res]   ; Load 'result' back into ST(0)

;     ; Convert to integer and store in 'integer_result'
;     fistp word [integer_res] ; Convert and pop ST(0)

;     ; Clear stack?
;     ; Realistically does nothing..
;     fstp
;     fstp

;     ; Draw pixel
;     mov ax, VIDEO_D_BUFFER
;     mov es, ax
;     mov di, word [integer_res]
;     mov word [es:di], 0x0F0F
;     ret


; C-like includes
%include "./draw/render.asm"
%include "./draw/keyboard.asm"
; %include "./draw/update_cre_data.asm"
; %include "./draw/text.asm"



section .text
code_segment_end:

times 4096-(code_segment_end - code_segment) db 0









section .data

align 4

; my_data equ 0x0900
my_data:





%include "./draw/render_data.asm"
%include "./draw/text.asm"
%include "./draw/keyboard_data.asm"


; section .data


; ; NEEDED TO DEFINE THE ARRAY HERE WITHOUT A SECTION-LABEL TO BE ABLE TO USE THE ARRAY!
word_array: dw 10, 20, 30, 40, 50 ; An array of 5 bytes
; ; word_buffer: resw 5 ; 10 bytes without specifying initial value

; byte_array: times 10 db 0 ; 10 bytes initaliezed to '0'
; byte_array_size: equ 10

; frame_buff: times 64 db 0 ; Cant use the 64k that I want for a full framebuffer..
; ; frame_buff_size: equ 640

; framebuffer_64k: 0x7E00
; Initialize buffer location
    ; mov ax, 0x7E00     ; Address right after boot sector
    ; mov es, ax         ; Set extra segment
    ; xor di, di         ; Zero offset
    ; mov cx, 10000        ; Number of bytes to clear
    ; xor al, al         ; Value to fill (zero)
    ; rep stosb          ; Clear the buffer



; PIXEL COORIDINATES
pixel_x dw 0
pixel_y dw 0


; player location
x dw 0
y dw 0
; mov WORD [y], 0
; mov WORD [x], 0

; player position : NOTE: INTIAL VALUE WILL ALWAYS BE 0 WHEN DEFINED IN .data!
player_position_x dw 0xA0
player_position_y dw 0x60
; mov word [player_position_x], 0xA0
; mov word [player_position_y], 0x60

; call render
; call render
; call render
; call render
; call render



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



;   Triangulation!
;   
; align 4
position dd 150.0, 80.0, 0.0
velocity dd 0.0, 0.0, 0.0
;
;


;
; GEOMETRY - fixed triangles that will be transformed and rendered
; 
; First triangle
; 36 bytes          ; x0, y0, z0  &  x1, y1, z1 & x2, y2, z2
triangle_1  dd      60.0,   60.0,   0.0, \
                    110.0,  80.0,   0.0, \
                    60.0,   110.0,  0.0


my_data_end: