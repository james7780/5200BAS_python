;---------------------------------------------------------------------
; CLS                           
;---------------------------------------------------------------------
CLS     LDY     #$00            
        LDA     #$00
        STA     TEMPL
        LDA     #$~O14<~
        STA     TEMPH
CNEXT   LDA     #$00            
        STA     (TEMPL),Y
        INY     
        CPY     #$00
        BNE     CNEXT
        INC     TEMPH
        LDA     TEMPH
        CMP     #$~O14D+1024H<~
        BNE     CNEXT
        RTS     
