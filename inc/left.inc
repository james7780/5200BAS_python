;---------------------------------------------------------------------
; LEFT -- moves the cursor left one column
;---------------------------------------------------------------------
LEFT    LDA     LOCATEL         
        BNE     LEFTA
        DEC     LOCATEH
LEFTA   DEC     LOCATEL         
        RTS     
