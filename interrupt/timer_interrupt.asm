[bits 16]
[org 0x7C00]            ; Boot sector start (if using a bootloader)

; Constants
PIT_CONTROL_PORT  equ 0x43
PIT_CHANNEL_0     equ 0x40
IRQ0_VECTOR       equ 0x08


; tick_count dw 5

; Set up video mode
; 0xA000 (0xA0000) = segment address for video memory
; 0xB800 (0xB8000) = segment address for text mode memory (mode 0x03)
; rememeber that when setting the video mode, text mode is disabled
mov ah, 0x00 ; set video mode
mov al, 0x13 ; 320x200 256-color mode
; mov al, 0x03 ; text mode
int 0x10

; initial screen clearing WITH CUSTOM COLORS
; Screen is automatically cleared when switching to video mode
mov ah, 0x06    ; Scroll up function
mov al, 0       ; Clear entire screen
mov bh, 0x08    ; dark gray
mov ch, 0       ; Upper left row
mov cl, 0       ; Upper left column
mov dh, 24      ; Lower right row
mov dl, 79      ; Lower right column
int 0x10        ; Call BIOS video interrupt


; ; Render a white pixel at (100, 50)
; mov ax, 0xA000         ; Segment for video memory
; mov es, ax             ; Point ES to video memory

; mov di, 50 * 320 + 100 ; Offset for pixel at (100, 50)
; mov al, 0x0F           ; Pixel color (bright white)
; mov [es:di], al        ; Write pixel color to video memory


; mov ah, 0x0C  ; BIOS video function: write pixel
; mov al, 0x0F  ; White color
; xor cx, cx
; xor dx, dx
; mov cx, 35  ; x
; mov dx, 10  ; y
; int 0x10

; Entry point
start:
    cli                         ; Disable interrupts during setup

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
    mov word [es:di], handler   ; Offset of custom handler
    mov word [es:di+2], cs      ; Segment of this code
    sti                         ; Re-enable interrupts

    ; Enable hardware interrupts
    ; mov al, 0xFE                ; Unmask IRQ0 (allow timer interrupts)
    ; out 0x21, al                ; Write to PIC mask register

    ; Infinite loop
main_loop:
    hlt                         ; Halt until next interrupt
    jmp main_loop

; Custom interrupt handler
handler:
    pusha                       ; Save all registers
    ; Your interrupt handling code here
    ; For example, increment a counter
    inc word [tick_count]

    ; mov ah, 0x06    ; Scroll up function
    ; mov al, 0       ; Clear entire screen
    ; mov bh, 0x08    ; dark gray
    ; mov ch, 0       ; Upper left row
    ; mov cl, 0       ; Upper left column
    ; mov dh, 24      ; Lower right row
    ; mov dl, 79      ; Lower right column
    ; int 0x10        ; Call BIOS video interrupt

    ; according to chatGPT this text-mode is incomaptible with graphics mode...
    ; So the below instructions is just a waste of execution in graphics mode
    ; mov ah, 0x00 ; set video mode
    ; mov al, 0x03 ; 320x200 256-color mode
    ; int 0x10
    mov ax, 0xB800               ; Video memory segment
    mov es, ax                   ; Load ES with video memory segment
    mov di, [tick_count]                   ; Offset for the top-left corner of the screen
    mov al, 'X'                  ; ASCII character to display
    mov ah, 0x0F                 ; Attribute: Bright white on black
    mov [es:di], ax              ; Write character + attribute to video memory

    ; clear screen
    mov ax, 0xA000         ; Segment for graphics video memory
    mov es, ax             ; Point ES to video memory
    mov di, 0              ; Start at the top-left corner
    mov cx, 320 * 200 / 2  ; Screen size in words (320x200 pixels = 64000 bytes)
    xor ax, ax             ; Color 0 (black)
    rep stosw              ; Fill video memory with color 0

    ; Render a white pixel at (100, 50)
    mov ax, 0xA000         ; Segment for video memory
    mov es, ax             ; Point ES to video memory

    mov di, [tick_count] ; Offset for pixel at (100, 50)
    ; mov di, 50 * 320 + 100 ; Offset for pixel at (100, 50)
    mov al, 0x0F           ; Pixel color (bright white)
    mov [es:di], al        ; Write pixel color to video memory
   
    ; add di, 2                    ; Move to the next position (each char takes 2 bytes)
    ; cmp di, 4000                 ; Check if we've reached the end of the screen (80x25 = 2000 characters, 4000 bytes)
    ; jl no_reset
    ; mov di, 0
    ; draw pixel
    ; mov ah, 0x0C  ; BIOS video function: write pixel
    ; mov al, 0x0F  ; White color
    ; xor cx, cx
    ; xor dx, dx
    ; ; inc word [tick_count]
    ; ; cmp word [boot_x], 6400
    ; ; jge .load_second_sector
    ; mov cx, [tick_count]  ; x
    ; mov dx, 10  ; y
    ; int 0x10

    ; call restore_irq0

    call test


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

test:
    mov ah, 0x0C  ; BIOS video function: write pixel
    mov al, 0x0F  ; White color
    xor cx, cx
    xor dx, dx
    mov cx, 35  ; x
    mov dx, 35  ; y
    int 0x10
    ret
     

; Data
old_irq0_offset dw 0x0000
old_irq0_segment dw 0x0000
tick_count dw 0

times 510-($-$$) db 0 ; Padding to make the boot sector 512 bytes
dw 0xAA55             ; Boot signature
