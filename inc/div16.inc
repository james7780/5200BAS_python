;---------------------------------------------------------------------
; DIV16 -- TEMPH:TEMPL remainder A = TEMPH:TEMPL/FROML
;---------------------------------------------------------------------
DIV16   LDX     #$10            
        LDA     #$00
        ASL     TEMPL
        ROL     TEMPH
DIV161  ROL     ~A~               
        CMP     FROML
        BCC     DIV162
        SBC     FROML
DIV162  ROL     TEMPL           
        ROL     TEMPH
        DEX     
        BNE     DIV161
        RTS     
