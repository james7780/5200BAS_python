;---------------------------------------------------------------------
; CHKROW -- if the cursor moved past the last row, scroll screen up
;---------------------------------------------------------------------
CHKROW  JSR     POS             
        CPX     #$18
        BCC     CHKROW1
        LDA     #$~O14D+920H<~
        STA     LOCATEH
        LDA     #$~O14D+920H>~
        STA     LOCATEL
        JSR     MOVEUP
CHKROW1 RTS                     
