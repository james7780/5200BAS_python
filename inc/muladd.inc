;---------------------------------------------------------------------
; MULADD -- (X:A)=A*Y+(X:TEMPL) if Y>0
;---------------------------------------------------------------------
MULADD  STA     TEMPH            
        LDA     #$00
MUL1    CPY     #$00             
        BEQ     MUL2
        DEY     
        CLC     
        ADC     TEMPH
        BCC     MUL1
        INX     
        JMP     MUL1
MUL2    CLC                     
        ADC     TEMPL
        BCC     MUL3
        INX     
MUL3    RTS                     
