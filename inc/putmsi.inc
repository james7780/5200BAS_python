;---------------------------------------------------------------------
; PUTMSI -- Clears missile memory column, then draws missile inverted
; Required on entry:            
;       [SPRITEL] = Page of sprite memory
;       [TEMPL]   = Page of sprite ROM graphics
;       FROML     = Vertical offset of sprite (0-159)
;       FROMH     = Height of missile
;       X         = Missile column (3,2,1,0)
; Changes:                      
;       A, Y, SPRITEL, TEMPL, FROMH/FROML, [SPRITEL]
;---------------------------------------------------------------------
PUTMSI  LDA     #$00            ; offset 0, also for writing $00
        STA     SPRITEL         ; from beginning of sprite memory page
        STA     TEMPL           ; from beginning of sprite graphics page
        TAY                     ; start at offset 0
PUTMSI1 LDA     (SPRITEL),Y     ; get byte of sprite memory
        AND     PUTMSI3,X       ; clear appropriate column
        STA     (SPRITEL),Y     ; save back the modified byte
        INY                     ; get ready to do next byte
        BNE     PUTMSI1         ; done clearing page? (Y=0 when done)
        LDA     FROML           ;\
        CLC                     ; \ put missile at vertical offset
        ADC     #$20            ; / 0-159 -> $20-$DF
        STA     SPRITEL         ;/
        STY     FROML
PUTMSI2 LDA     (TEMPL),Y       ; char to fill with from missile graphics mem
        AND     PUTMSI4,X       ; clear other columns
        ORA     (SPRITEL),Y     ; add with old byte
        STA     (SPRITEL),Y     ; write char to missile memory
        INC     FROML           ; get ready to do next byte
        DEC     FROMH           ;/
        BPL     PUTMSI2         ; done moving? if not, keep going
        RTS                     ; all done, exit
PUTMSI3 .BYTE   $FC,$F3,$CF,$3F 
PUTMSI4 .BYTE   $03,$0C,$30,$C0 
