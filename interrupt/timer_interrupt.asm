[bits 16]
[org 0x7C00]            ; Boot sector start (if using a bootloader)

; Constants
PIT_CONTROL_PORT  equ 0x43
PIT_CHANNEL_0     equ 0x40
IRQ0_VECTOR       equ 0x08

; Interrupt Data
old_irq0_offset dw 0x0000
old_irq0_segment dw 0x0000



; Set up video mode
; 0xA000 (0xA0000) = segment address for video memory
; 0xB800 (0xB8000) = segment address for text mode memory (mode 0x03)
; rememeber that when setting the video mode, text mode is disabled
; Screen is automatically cleared when switching to video mode
mov ah, 0x00 ; set video mode
mov al, 0x13 ; 320x200 256-color mode
; mov al, 0x03 ; text mode
int 0x10



setup:
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
    mov ax, [es:di]        ; Load offset into AX
    mov [old_irq0_offset], ax
    mov ax, [es:di+2]      ; Load segment into AX
    mov [old_irq0_segment], ax

    ; Set new handler
    ; cli
    mov word [es:di], timer_handler   ; Offset of custom handler
    mov word [es:di+2], cs      ; Segment of this code
    sti                         ; Re-enable interrupts



main_loop:
    hlt                         ; Halt until next interrupt
    jmp main_loop



tick_count dw 0

timer_handler:
    pusha
    inc word [tick_count]


    ; clear screen using graphics mode writing directly to video buffer
    mov ax, 0xA000         ; Segment for graphics video memory
    mov es, ax             ; Point ES to video memory
    mov di, 0              ; Start at the top-left corner
    mov cx, 320 * 200 / 2  ; rep stosw increments two bytes per iteration
    mov ax, 0xCFCF ; 2 x mode 13h color palette (1 byte / pixel)
    
    ; Increments di by 2 each iteration (default DF=0) and loads eax into [es:di] and stops at di=cx?
    ; stosw : w=word=eax, stosb : w=byte,
    rep stosw


    ; Render a white pixel at (100, 50)
    mov ax, 0xA000         ; Segment for video memory
    mov es, ax             ; Point ES to video memory

    mov di, [tick_count] ; Offset for pixel at (100, 50)
    ; mov di, 50 * 320 + 100 ; Offset for pixel at (100, 50)
    mov al, 0x0F           ; Pixel color (bright white)
    mov [es:di], al        ; Write pixel color to video memory


    ; EOI command making sure proper end of interrupt?
    mov al, 0x20                ; EOI command
    out 0x20, al                ; Send to PIC command port (0x20)

    ; End of handler
    popa                       ; Restore all registers
    iret                      ; Return from interrupt


; Restore original IRQ0 handler
restore_irq0:
    cli
    mov ax, 0x0000
    mov es, ax
    mov di, IRQ0_VECTOR * 4
    mov ax, [old_irq0_offset]
    mov [es:di], ax
    mov ax, [old_irq0_segment]
    mov word [es:di+2], ax
    sti
    ret



; Boot sector padding
times 510-($-$$) db 0 ; Padding to make the boot sector 512 bytes
dw 0xAA55             ; Boot signature
