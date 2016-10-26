;---------------------------------------------------------------------
; RIGHT -- moves the cursor right one column
;---------------------------------------------------------------------
RIGHT   INC     LOCATEL         
        BNE     RIGHTA
        INC     LOCATEH
RIGHTA  RTS                     
