;---------------------------------------------------------------------
; INPUT                         
; Requires: KEY (last key pressed, $FF=none)
; Returns:  keypress in A       
;---------------------------------------------------------------------
INPUT   LDA     KEY             ; get current key status
        CMP     #$FF            ; key pressed?
        BEQ     INPUT           ; if not, loop and wait for input
        LDX     #$FF
        STX     KEY
        RTS     
