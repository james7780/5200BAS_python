;-----------------------------------------------------------
; Clear RAM                     
;-----------------------------------------------------------
CLRRAM  LDA     #$00            
        STA     ($80),Y         ;Store data
        INY                     ;Next byte
        BNE     CLRRAM          ;Loop if this page isn't done
        INC     $81             ;Do the next page
        LDA     $81
        CMP     #$40            ;Check if end of RAM
        BNE     CLRRAM          ;Keep going if not
        RTS
