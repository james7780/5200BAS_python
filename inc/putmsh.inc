;---------------------------------------------------------------------
; PUTMSH -- Clears missile memory column, then draws missile
; Required on entry:            
;       [SPRITEL] = Page of sprite memory
;       [TEMPL]   = Page of sprite ROM graphics
;       FROML     = Vertical offset of sprite (0-159)
;       FROMH     = Height of missile
;       X         = Missile column (3,2,1,0)
; Changes:                      
;       A, Y, SPRITEL, TEMPL, [SPRITEL]
;---------------------------------------------------------------------
PUTMSH  LDA     #$00            ; offset 0, also for writing $00
        STA     SPRITEL         ; from beginning of sprite memory page
        STA     TEMPL           ; from beginning of sprite graphics page
        TAY                     ; start at offset 0
PUTMSH1 LDA     (SPRITEL),Y     ; get byte of sprite memory
        AND     PUTMSH3,X       ; clear appropriate column
        STA     (SPRITEL),Y     ; save back the modified byte
        INY                     ; get ready to do next byte
        BNE     PUTMSH1         ; done clearing page? (Y=0 when done)
        LDA     FROML           ;\
        CLC                     ; \ put missile at vertical offset
        ADC     #$20            ; / 0-159 -> $20-$DF
        STA     SPRITEL         ;/
PUTMSH2 LDA     (TEMPL),Y       ; char to fill with from missile graphics mem
        AND     PUTMSH4,X       ; clear other columns
        ORA     (SPRITEL),Y     ; add with old byte
        STA     (SPRITEL),Y     ; write char to missile memory
        INY                     ; get ready to do next byte
        CPY     FROMH           ; done moving?
        BNE     PUTMSH2         ; if not, keep going
        RTS                     ; all done, exit
PUTMSH3 .BYTE   $FC,$F3,$CF,$3F 
PUTMSH4 .BYTE   $03,$0C,$30,$C0 
