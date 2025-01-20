;   
;   A raw binary that renders a pixel for every keyboard stroke - this comment was VERY valuable!
;

; RUN
;
; // source : Claude
; nasm -f bin -o main.bin main.asm
; qemu-system-i386 -drive format=raw,file=main.bin 

; This one from GPT also works! Probably the same BIOS?
; qemu-system-x86_64 -drive format=raw,file=main.bin // GPT

; DISASSEMBLE
; objdump -b binary -m i386 -D main.bin



; Good updates on 2024-10-31
; https://claude.ai/chat/31ae4818-1251-4fc4-ae3d-a7d2fb784b69
; Yet to implement the timer!  or "draw state"


; DATA
; https://cratecode.com/info/x86-nasm-assembly-array-manipulation
; section .data
    ; my_array: dw 10, 20, 30, 40, 50 ; An array of 5 bytes
    ; array_length equ 5        ; Define the length of the array

; section .text
; mov eax, [my_array + 2] ; Move the third element of my_array into eax

; Claude : First, make sure DS is set up correctly
; mov ax, ds               ; Save current DS
; mov bx, cs               ; Get code segment
; mov ds, bx               ; Set DS to same segment as code

%ifdef ELF
section .text
global _start
_start:
%else
[bits 16]
[org 0x7c00]
%endif


; Claude

; [bits 16]
; [org 0x7c00]



; Set up video mode (320x200, 256 colors)
mov ax, 0x13
int 0x10

; Set up keyboard interrupt handler
cli
mov word [0x24], keyboard_handler
mov word [0x26], 0
sti

; NEEDED TO DEFINE THE ARRAY HERE WITHOUT A SECTION-LABEL TO BE ABLE TO USE THE ARRAY!
word_array: dw 10, 20, 30, 40, 50 ; An array of 5 bytes
; word_buffer: resw 5 ; 10 bytes without specifying initial value

byte_array: times 10 db 0 ; 10 bytes initaliezed to '0'
byte_array_size: equ 10

frame_buff: times 64 db 0 ; Cant use the 64k that I want for a full framebuffer..
; frame_buff_size: equ 640

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

; Main loop
main_loop:
    hlt
    jmp main_loop




; Keyboard interrupt handler
; Will break the main loop and update the rendered content
keyboard_handler:

    pusha ; Need .286 direcctive? - 2024-10-31 - https://stackoverflow.com/questions/29728171/x86-assembly-set-of-pushes-and-pusha-difference
    in al, 0x60  ; Read keyboard scancode


    ; Read keyboard status & prevent double-handle when pressing key. The follwoing lines prevents handling of key release.
    ; in al, 0x60        ; Read scan code ; Already done above!
    test al, 0x80      ; Test if this is a key release (bit 7 set)
    jz .done          ; If it's a release, skip drawing

    
    call .clear_screen

    call .draw_large_square
    
    call .update_location
    ; in al, 0x60  ; Read keyboard scancode/
    ; mov cl, al ; trying to detect keyboard input
    ; mov cx, ax ; trying to detect keyboard input

    call .draw_current_location

    jmp .done



.draw_current_location:
; Draw the 'current' pixel
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0F  ; White color
    mov cx, [pixel_x]
    mov dx, [pixel_y]
    int 0x10
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
    ret

; Draw large square
.draw_large_square:
    mov bh, 0x06    ; coor
    mov ch, 10      ; Upper left row of square
    mov cl, 10      ; Upper left column of square
    mov dh, 20      ; Lower right row of square
    mov dl, 20      ; Lower right column of square
    int 0x10        ; Call BIOS video interrupt
    ret


; Clear the screen
.clear_screen:
    mov ah, 0x06    ; Scroll up function
    mov al, 0       ; Clear entire screen
    mov bh, 0x00    ; Black background
    mov ch, 0       ; Upper left row
    mov cl, 0       ; Upper left column
    mov dh, 24      ; Lower right row
    mov dl, 79      ; Lower right column
    int 0x10        ; Call BIOS video interrupt
    ret
    

.done:

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
    
    mov al, 0x20
    out 0x20, al  ; Send EOI to PIC
    popa
    iret ; interrupt - meaning : 2024-10-31


; times 510-($-$$) db 0
; dw 0xAA55

%ifndef ELF
times 510-($-$$) db 0
dw 0xaa55
%endif


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