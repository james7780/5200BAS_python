;---------------------------------------------------------------------
; PUTSPI -- Clears sprite memory, then draws inverted sprite to memory
; Required on entry:            
;       [SPRITEL] = Page of sprite memory
;       [TEMPL]   = Page of sprite ROM graphics
;       FROMH     = Height of sprite
;       X         = Vertical offset of sprite (0-159)
; Changes:                      
;       A, Y, SPRITEL, FROMH/FROML, TEMPL, [SPRITEL]
;---------------------------------------------------------------------
PUTSPI  LDA     #$00            ; offset 0, also for writing $00
        STA     SPRITEL         ; from beginning of sprite memory page
        STA     TEMPL           ; from beginning of sprite graphics page
        TAY                     ; start at offset 0
PUTSPI1 STA     (SPRITEL),Y     ; clear byte of sprite memory
        INY                     ; get ready to do next byte
        BNE     PUTSPI1         ; done clearing page? (Y=0 when done)
        TXA                     ;\
        CLC                     ; \ put sprite at vertical offset
        ADC     #$20            ; / 0-159 -> $20-$DF
        STA     SPRITEL         ;/
        STY     FROML
PUTSPI2 LDY     FROMH
        LDA     (TEMPL),Y       ; char to fill with from sprite graphics mem
        LDY     FROML
        STA     (SPRITEL),Y     ; write char to sprite memory
        INC     FROML           ; get ready to do next byte
        DEC     FROMH           ;/
        BPL     PUTSPI2         ; done moving? if not, keep going
        RTS                     ; all done, exit
