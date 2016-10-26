;---------------------------------------------------------------------
; MEMCOPY                       
;---------------------------------------------------------------------
MEMCOPY LDY     #$00            
MEMCPYA LDA     (FROML),Y       
        STA     (TOL),Y
        LDA     COUNTH
        CMP     #$00
        BNE     MEMCPY1
        LDA     COUNTL
        CMP     #$00
        BEQ     MEMCPYD
MEMCPY1 DEC     COUNTL          
        LDA     COUNTL
        CMP     #$FF
        BNE     MEMCPY2
        DEC     COUNTH
MEMCPY2 INY                     
        BNE     MEMCPYA
        INC     FROMH
        INC     TOH
        JMP     MEMCPYA
MEMCPYD RTS                     
