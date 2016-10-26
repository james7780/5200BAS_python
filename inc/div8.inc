;---------------------------------------------------------------------
; DIV8 -- TEMPL remainder A = TEMPL/TEMPH
; from public domain code by Todd P. Whitesel
;---------------------------------------------------------------------
DIV8    LDX     #$08            
        LDA     #$00
        ASL     TEMPL
DIV81   ROL     ~A~               
        CMP     TEMPH
        BCC     DIV82
        SBC     TEMPH
DIV82   ROL     TEMPL           
        DEX     
        BNE     DIV81
        RTS     
