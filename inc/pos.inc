;---------------------------------------------------------------------
; POS -- cursor position, Returns: X=row, A=column
;---------------------------------------------------------------------
POS     LDA     #$28            
        STA     FROML
        LDA     LOCATEH
        SEC     
        SBC     #$~O14<~
        STA     TEMPH
        LDA     LOCATEL
        STA     TEMPL
        JSR     DIV16           ; TEMPH:TEMPLrA=TEMPH:TEMPL/FROML
        LDX     TEMPL
        RTS     
