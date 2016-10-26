;---------------------------------------------------------------------
; MOVEUP -- scrolls the screen up by one line, clears the bottom row
;---------------------------------------------------------------------
MOVEUP  LDY     #$00             
        LDA     #$~O14<~
        STA     TEMPH
MUNEXT  LDX     #$28            ;get character from row+1
        STX     TEMPL
        LDA     (TEMPL),Y
        LDX     #$00
        STX     TEMPL
        STA     (TEMPL),Y       ;store character in row (effect=-1 row)
        INY     
        CPY     #$00
        BNE     MUNEXT
        INC     TEMPH
        LDA     TEMPH
        CMP     #$14
        BNE     MUNEXT
        LDA     #$~O14D+920H<~            ;set for last line
        STA     TEMPH
        LDA     #$~O14D+920H>~
        STA     TEMPL
        LDY     #$00
        TYA     
MUNEXT2 STA     (TEMPL),Y       ;clear a character
        INY     
        CPY     #$28
        BNE     MUNEXT2
        RTS     
