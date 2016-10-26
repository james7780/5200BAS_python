;---------------------------------------------------------------------
; PUTMSL -- Clears missile memory column, then draws missile
; Required on entry:            
;       [SPRITEL] = Page of sprite memory
;       [TEMPL]   = Page of sprite ROM graphics
;       FROML     = Vertical offset of sprite (0-159)
;       X         = Missile column (3,2,1,0)
; Changes:                      
;       A, Y, SPRITEL, TEMPL, [SPRITEL]
;---------------------------------------------------------------------
PUTMSL  LDA     #$00            ; offset 0, also for writing $00
        STA     SPRITEL         ; from beginning of sprite memory page
        STA     TEMPL           ; from beginning of sprite graphics page
        TAY                     ; start at offset 0
PUTMSL1 LDA     (SPRITEL),Y     ; get byte of sprite memory
        AND     PUTMSL3,X       ; clear appropriate column
        STA     (SPRITEL),Y     ; save back the modified byte
        INY                     ; get ready to do next byte
        BNE     PUTMSL1         ; done clearing page? (Y=0 when done)
        LDA     FROML           ;\
        CLC                     ; \ put missile at vertical offset
        ADC     #$20            ; / 0-159 -> $20-$DF
        STA     SPRITEL         ;/
PUTMSL2 LDA     (TEMPL),Y       ; char to fill with from missile graphics mem
        AND     PUTMSL4,X       ; clear other columns
        ORA     (SPRITEL),Y     ; add with old byte
        STA     (SPRITEL),Y     ; write char to missile memory
        INY                     ; get ready to do next byte
        BNE     PUTMSL2         ; done moving?
        RTS                     ; all done, exit
PUTMSL3 .BYTE   $FC,$F3,$CF,$3F 
PUTMSL4 .BYTE   $03,$0C,$30,$C0 
