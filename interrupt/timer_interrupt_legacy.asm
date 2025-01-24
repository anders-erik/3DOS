
;
;   OLD ATTEMPT AT TIMER INTERRUPT THAT NEVER WORKED
;



; Set up interrupt descriptor table for timer interrupt

; idt: times 256 dq 0  ; Allocate space for 256 IDT entries (8 bytes each)

; idt_ptr:
;     dw idt_end - idt - 1  ; Limit (size of IDT - 1)
;     dd idt                ; Base address of IDT

; idt_end:




; start:
;     cli                  ; Disable interrupts
;     ; Initialize IDT entry for IRQ0 (interrupt 0x08)
;     ; Assuming code segment selector is 0x08
;     mov eax, timer_interrupt
;     mov word [idt + 8*0x08], ax
;     mov word [idt + 8*0x08 + 6], 0x08
;     mov byte [idt + 8*0x08 + 5], 0x8E  ; Present, DPL=0, 32-bit interrupt gate
;     shr eax, 16
;     mov word [idt + 8*0x08 + 2], ax

; ;     ; Load IDT
;     ; BREAKS THE PROGRAM!
;     ; Constantly resets the VM/program
;     ; lidt [idt_ptr] 

; ;     ; Set PIT to Channel 0, LSB+MSB, Mode 3
;     mov al, 0x36
;     out 0x43, al

; ;     ; Load divisor for 1 Hz (PIT clock ~1.19318MHz / 1)
;     mov ax, 1193180
;     out 0x40, al        ; Low byte
;     mov al, ah
;     out 0x40, al        ; High byte

;     sti                 ; Enable interrupts

;     ; Or keyboard interrupts won't work properly
;     xor ax, ax


; Timer interrupt handler
timer_interrupt:
    pusha
    inc word [x]
    popa
    iret  ; Return from interrupt