;
;   A raw binary that renders a pixel for every keyboard stroke
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



; Claude

[bits 16]
[org 0x7c00]

; Set up video mode (320x200, 256 colors)
mov ax, 0x13
int 0x10

; Set up keyboard interrupt handler
cli
mov word [0x24], keyboard_handler
mov word [0x26], 0
sti

; Main loop
main_loop:
    hlt
    jmp main_loop

; Keyboard interrupt handler
keyboard_handler:
    pusha
    in al, 0x60  ; Read keyboard scancode

    ; Draw a pixel
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0F  ; White color
    mov cx, [pixel_x]
    mov dx, [pixel_y]
    int 0x10

    ; Update pixel position
    inc word [pixel_x]
    cmp word [pixel_x], 320
    jl .done
    mov word [pixel_x], 0
    inc word [pixel_y]
    cmp word [pixel_y], 200
    jl .done
    mov word [pixel_y], 0

.done:
    mov al, 0x20
    out 0x20, al  ; Send EOI to PIC
    popa
    iret

pixel_x dw 0
pixel_y dw 0

times 510-($-$$) db 0
dw 0xAA55


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