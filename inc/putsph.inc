;---------------------------------------------------------------------
; PUTSPH -- Clears sprite memory, and draws sprite in sprite memory
; Required on entry:            
;       [SPRITEL] = Page of sprite memory
;       [TEMPL]   = Page of sprite ROM graphics
;       FROMH     = Height of sprite
;       X         = Vertical offset of sprite (0-159)
; Changes:                      
;       A, Y, SPRITEL, TEMPL, [SPRITEL]
;---------------------------------------------------------------------
PUTSPH  LDA     #$00            ; offset 0, also for writing $00
        STA     SPRITEL         ; from beginning of sprite memory page
        STA     TEMPL           ; from beginning of sprite graphics page
        TAY                     ; start at offset 0
PUTSPH1 STA     (SPRITEL),Y     ; clear byte of sprite memory
        INY                     ; get ready to do next byte
        BNE     PUTSPH1         ; done clearing page? (Y=0 when done)
        TXA                     ;\
        CLC                     ; \ put sprite at vertical offset
        ADC     #$20            ; / 0-159 -> $20-$DF
        STA     SPRITEL         ;/
PUTSPH2 LDA     (TEMPL),Y       ; char to fill with from sprite graphics mem
        STA     (SPRITEL),Y     ; write char to sprite memory
        INY                     ; get ready to do next byte
        CPY     FROMH           ; done moving?
        BNE     PUTSPH2         ; if not, keep going
        RTS                     ; all done, exit
