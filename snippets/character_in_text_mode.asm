; according to chatGPT this text-mode is incomaptible with graphics mode...
; So the below instructions is just a waste of execution in graphics mode

; Set text mode
mov ah, 0x00 ; 
mov al, 0x03 ; 
int 0x10

; Draw white 'X'
mov ax, 0xB800               ; Video memory segment
mov es, ax                   ; Load ES with video memory segment
mov di, 10                   ; Offset for the top-left corner of the screen
mov al, 'X'                  ; ASCII character to display
mov ah, 0x0F                 ; Attribute: Bright white on black
mov [es:di], ax              ; Write character + attribute to video memory