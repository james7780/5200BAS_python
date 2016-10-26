;---------------------------------------------------------------------
; MEMAREA                       
;---------------------------------------------------------------------
MEMAREA LDY     #$00            
MEMARE0 LDA     (FROML),Y       
        STA     (TOL),Y
        LDA     COUNTH
        CMP     #$00
        BNE     MEMARE1
        LDA     COUNTL
        CMP     #$00
        BEQ     MEMARED
MEMARE1 DEC     COUNTL          
        LDA     COUNTL
        CMP     #$FF
        BNE     MEMARE2
        DEC     COUNTH
MEMARE2 INY                     
        CPY     COPYLEN
        BNE     MEMARE0
        LDY     #$00
        LDA     FROML
        CLC     
        ADC     COPYLEN
        BCC     MEMARE3
        INC     FROMH
MEMARE3 STA     FROML           
        LDA     TOL
        CLC     
        ADC     COPYLEN
        BCC     MEMARE4
        INC     TOH
MEMARE4 CLC                     
        ADC     SKIP
        BCC     MEMARE5
        INC     TOH
MEMARE5 STA     TOL             
        JMP     MEMARE0
MEMARED RTS                     
