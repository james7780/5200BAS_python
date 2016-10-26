;---------------------------------------------------------------------
; PRINT                         
;---------------------------------------------------------------------
PRINT   LDY     #$00            
PNEXT   LDA     (TEMPL),Y       
        CMP     #$FF
        BEQ     PDONE
        STA     (LOCATEL),Y
        INY     
        JMP     PNEXT
PDONE   RTS                     
